{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
  Module      :  System.Posix.Syslog.UDP
  Maintainer  :  jon@childr.es
  Stability   :  provisional
  Portability :  Posix

  Log messages to syslog over a network via UDP, with protocols such as
  <https://tools.ietf.org/html/rfc5424 RFC 5424> or
  <https://tools.ietf.org/html/rfc3164 RFC 3164>.

  The following features are currently missing (but may be provided in future
  releases):

    * validation of string identifiers such as @APP-NAME@ or @MSGID@
    * support for @STRUCTURED-DATA@ (RFC 5424 only)
-}

module System.Posix.Syslog.UDP
  ( -- * Syslog UDP packet component datatypes
    -- ** Re-exports from <http://hackage.haskell.org/package/hsyslog hsyslog>
    L.Priority (..)
  , L.Facility (..)
  , L.PriorityMask (..)
    -- ** Newtypes for various String/Int values
    -- | Refer to
    -- <https://tools.ietf.org/html/rfc5424#section-6.2 RFC 5424 section 6.2>
    -- as to the purpose of each.
  , AppName (..)
  , HostName (..)
  , PriVal (..)
  , ProcessID (..)
  , MessageID (..)
    -- ** Type aliases
    -- | What syslog refers to as 'L.Priority',
    -- <https://tools.ietf.org/html/rfc5424 RFC 5424> calls 'Severity'.
  , Severity
  , SeverityMask
  -- ** Structured Data
  -- | Currently unsupported; a placeholder for future use.
  , StructuredData (..)
    -- * The easy Haskell API to syslog via UDP
  , initSyslog
  , SyslogFn
  , SyslogConfig (..)
  , defaultConfig
  , localhost
    -- ** Common protocols for use with 'SyslogConfig'
  , Protocol
  , rfc5424Protocol
  , rfc3164Protocol
  , rsyslogProtocol
    -- * Manually constructing syslog UDP packets
  , rfc5424Packet
  , rfc3164Packet
  , rsyslogPacket
    -- ** Miscellaneous utilities
  , getAppName
  , getHostName
  , getProcessId
  , maskedPriVal
  , resolveUdpAddress
  , rfc3339Timestamp
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Foreign.C (CInt)
import System.Environment (getProgName)
import System.Posix.Types (CPid (..))

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T
import qualified Network.BSD as BSD
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import qualified System.Posix.Process as P
import qualified System.Posix.Syslog as L

type Severity = L.Priority
type SeverityMask = L.PriorityMask

type Protocol
  =  PriVal
  -> UTCTime
  -> HostName
  -> AppName
  -> ProcessID
  -> Text
  -> ByteString

class ToLogValue a where
  toLogValue :: a -> ByteString

instance ToLogValue ByteString where
  toLogValue bs | B.null bs = nilValue
  toLogValue bs = bs

instance ToLogValue Text where
  toLogValue = toLogValue . T.encodeUtf8

instance ToLogValue a => ToLogValue (Maybe a) where
  toLogValue Nothing = nilValue
  toLogValue (Just x) = toLogValue x

newtype AppName
  = AppName ByteString
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.5 APP-NAME>@
  deriving (Eq, Show)

instance ToLogValue AppName where
  toLogValue (AppName x) = toLogValue x

newtype HostName
  = HostName ByteString
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.4 HOSTNAME>@;
  -- fetch via 'getHostName'
  deriving (Eq, Show)

instance ToLogValue HostName where
  toLogValue (HostName x) = toLogValue x

newtype PriVal
  = PriVal CInt
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.1 PRI>@;
  -- construct via 'maskedPriVal'
  deriving (Eq, Show)

instance ToLogValue PriVal where
  toLogValue = formatPriVal

newtype ProcessID
  = ProcessID ByteString
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.6 PROCID>@;
  -- fetch via 'getProcessId'
  deriving (Eq, Show)

instance ToLogValue ProcessID where
  toLogValue (ProcessID x) = toLogValue x

newtype MessageID
  = MessageID ByteString
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.7 MSGID>@
  deriving (Eq, Show)

instance ToLogValue MessageID where
  toLogValue (MessageID x) = toLogValue x

data StructuredData
  = StructuredData
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.3 STRUCTURED-DATA>@
  -- (unsupported)

-- | Return a function that logs to syslog via UDP.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import System.Posix.Syslog.UDP
-- >
-- > main = do
-- >   syslog <- defaultConfig >>= initSyslog
-- >   putStrLn "huhu"
-- >   syslog USER Debug "huhu"
--
-- This makes no assumptions about socket connection status or endpoint
-- availability. Any errors while sending are silently ignored.

initSyslog :: SyslogConfig -> IO SyslogFn
initSyslog config = S.withSocketsDo $ do
    socket <- S.socket (S.addrFamily $ address config) S.Datagram udpProtoNum
    let send = flip (SB.sendTo socket) (S.addrAddress $ address config)

    return $ \facility severity message ->
      case maskedPriVal (severityMask config) facility severity of
        Nothing -> return ()
        Just priVal -> do
          time <- getCurrentTime
          safely . send $ (protocol config) priVal time (hostName config)
            (appName config) (processId config) message

-- | The type of function returned by 'initSyslog'.

type SyslogFn
  =  L.Facility -- ^ facility to log to
  -> Severity   -- ^ severity under which to log
  -> Text       -- ^ message body
  -> IO ()

-- | Configuration options for connecting and logging to your syslog socket.

data SyslogConfig = SyslogConfig
  { appName :: !AppName
    -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.5 APP-NAME>@;
    -- fetch via 'getAppName'
  , hostName :: !HostName
    -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.4 HOSTNAME>@;
    -- fetch via 'getHostName'
  , processId :: !ProcessID
    -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.6 PROCID>@;
    -- fetch via 'getProcessId'
  , severityMask :: !SeverityMask
    -- ^ whitelist of priorities of logs to send
  , address :: !S.AddrInfo
    -- ^ where to send the syslog packets; construct via 'resolveUdpAddress' or
    -- find via 'S.getAddrInfo'
  , protocol :: Protocol
    -- ^ protocol for formatting the message, such as 'rfc5424Protocol' or
    -- 'rfc3164Protocol'
  }

-- | A convenient default config for connecting to 'localhost'. Provided for
-- development/testing purposes.

defaultConfig :: IO SyslogConfig
defaultConfig = do
    appName <- getAppName
    hostName <- getHostName
    processId <- getProcessId
    return SyslogConfig {..}
  where
    severityMask = L.NoMask
    address = localhost
    protocol = rfc3164Protocol

-- | The default IPv4 address/port for syslog on a local machine. Provided for
-- development/testing purposes.

localhost :: S.AddrInfo
localhost =
    S.AddrInfo
      { S.addrFlags = []
      , S.addrFamily = S.AF_INET
      , S.addrSocketType = S.Datagram
      , S.addrProtocol = udpProtoNum
      , S.addrAddress = S.SockAddrInet 514 16777343
      , S.addrCanonName = Nothing
      }

-- | Construct a syslog UDP packet as dictated by
-- <https://tools.ietf.org/html/rfc5424 RFC 5424>. Note that fields in a syslog
-- packet are whitespace-delineated, so don't allow whitespace in anything but
-- the log message!

rfc5424Packet
  :: FormatTime t
  => PriVal
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.1 PRI>@;
  -- construct via 'maskedPriVal'
  -> Maybe t
  -- ^ time of message, converted to
  -- @<https://tools.ietf.org/html/rfc5424#section-6.2.3 TIMESTAMP>@
  -> Maybe HostName
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.4 HOSTNAME>@;
  -- fetch via 'getHostName'
  -> Maybe AppName
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.5 APP-NAME>@;
  -- fetch via 'getAppName'
  -> Maybe ProcessID
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.6 PROCID>@;
  -- fetch via 'getProcessId'
  -> Maybe MessageID
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.7 MSGID>@
  -> Maybe StructuredData
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.3 STRUCTURED-DATA>@
  -- (unsupported)
  -> Text
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.4 MSG>@
  -> ByteString
rfc5424Packet priVal time hostName' appName' processId' messageId _ message =
         toLogValue priVal
     <>  version
    `sp` maybe nilValue rfc3339Timestamp time
    `sp` toLogValue hostName'
    `sp` toLogValue appName'
    `sp` toLogValue processId'
    `sp` toLogValue messageId
    `sp` structData
    `sp` toLogValue message
  where
    version = "1"
    structData = nilValue

rfc5424Protocol :: Protocol
rfc5424Protocol priVal time hostName' appName' processId' message =
    rfc5424Packet priVal (Just time) (Just hostName') (Just appName')
      (Just processId') Nothing Nothing message

-- | Construct a syslog UDP packet as dictated by
-- <https://tools.ietf.org/html/rfc3164 RFC 3164>. Note that fields in a syslog
-- packet are whitespace-delineated, so don't allow whitespace in anything but
-- the log message!

rfc3164Packet
  :: FormatTime t
  => PriVal
  -- ^ see @<https://tools.ietf.org/html/rfc3164#section-4.1.1 PRI>@;
  -- construct via 'maskedPriVal'
  -> t
  -- ^ time of message, converted to @TIMESTAMP@ in
  -- @<https://tools.ietf.org/html/rfc3164#section-4.1.2 HEADER>@
  -> HostName
  -- ^ the @HOSTNAME@ of the
  -- @<https://tools.ietf.org/html/rfc3164#section-4.1.2 HEADER>@;
  -- fetch via 'getHostName'
  -> AppName
  -- ^ the program name in the @TAG@ portion of the
  -- @<https://tools.ietf.org/html/rfc3164#section-4.1.3 MSG>@; fetch via
  -- 'getAppName'
  -> ProcessID
  -- ^ the process identifier in the @TAG@ portion of the
  -- @<https://tools.ietf.org/html/rfc3164#section-4.1.3 MSG>@; fetch via
  -- 'getProcessId'
  -> Text
  -- ^ the @CONTENT@ portion of the
  -- @<https://tools.ietf.org/html/rfc3164#section-4.1.3 MSG>@
  -> ByteString
rfc3164Packet = rfc3164Variant timeFormat
  where
    timeFormat = B.pack . formatTime defaultTimeLocale "%b %e %X"

rfc3164Protocol :: Protocol
rfc3164Protocol = rfc3164Packet

-- | Recommended rsyslog template
-- @<http://www.rsyslog.com/doc/v8-stable/configuration/templates.html RSYSLOG_ForwardFormat>@.
-- Same fields as RFC 3164, but with an RFC 3339 high-precision timestamp.

rsyslogPacket
  :: FormatTime t
  => PriVal
  -> t
  -> HostName
  -> AppName
  -> ProcessID
  -> Text
  -> ByteString
rsyslogPacket = rfc3164Variant rfc3339Timestamp

rsyslogProtocol :: Protocol
rsyslogProtocol = rsyslogPacket

-- | An <https://tools.ietf.org/html/rfc3339 RFC 3339> high-precision
-- timestamp.

rfc3339Timestamp :: FormatTime t => t -> ByteString
rfc3339Timestamp = B.pack . formatTime defaultTimeLocale "%FT%X%QZ"

-- | Obtain an IPv4 'S.AddrInfo' for your 'SyslogConfig' from a hostname
-- (or IPv4 address) and port. Sets the address protocol to 'S.Datagram'.

resolveUdpAddress :: Integral n => String -> n -> IO (Maybe S.AddrInfo)
resolveUdpAddress name port = do
    host <- BSD.getHostByName name
    return $ case BSD.hostAddresses host of
      (h:_) ->
        Just S.AddrInfo
          { S.addrFlags = []
          , S.addrFamily = BSD.hostFamily host
          , S.addrSocketType = S.Datagram
          , S.addrProtocol = udpProtoNum
          , S.addrAddress = S.SockAddrInet (fromIntegral port) h
          , S.addrCanonName = Nothing
          }
      _ ->
        Nothing

getAppName :: IO AppName
getAppName = AppName . B.pack <$> getProgName

getHostName :: IO HostName
getHostName = HostName . B.pack <$> BSD.getHostName

getProcessId :: IO ProcessID
getProcessId = do
    (CPid pid) <- P.getProcessID
    return . ProcessID . B.pack $ show pid

-- | Construct a @<https://tools.ietf.org/html/rfc5424#section-6.2.1 PRI>@.
-- 'Nothing' indicates that the severities are fully masked, and so no packet
-- should be sent.

maskedPriVal
  :: SeverityMask
  -> L.Facility
  -> Severity
  -> Maybe PriVal
maskedPriVal mask fac sev
    | mask == L.NoMask = prival
    | masked = Nothing
    | otherwise = prival
  where
    prival = Just . PriVal $ L.makePri fac sev
    masked = L._LOG_MASK (L.fromPriority sev) .&. L.fromPriorityMask mask == 0

-- internal functions

formatPriVal :: PriVal -> ByteString
formatPriVal (PriVal x) = "<" <> B.pack (show x) <> ">"

nilValue :: ByteString
nilValue = "-"

rfc3164Variant
  :: (t -> ByteString)
  -> PriVal
  -> t
  -> HostName
  -> AppName
  -> ProcessID
  -> Text
  -> ByteString
rfc3164Variant timeFormat priVal time hostName' appName' processId' message =
         toLogValue priVal
     <>  timeFormat time
    `sp` toLogValue hostName'
    `sp` mkTag appName' processId'
    `sp` toLogValue message
  where
    mkTag (AppName name) (ProcessID procId) = name <> "[" <> procId <> "]:"

safely :: IO a -> IO ()
safely f = catch (void f) (const $ return () :: SomeException -> IO ())

sp :: ByteString -> ByteString -> ByteString
sp b1 b2 = b1 <> " " <> b2
{-# INLINE sp #-}

-- see http://www.iana.org/assignments/protocol-numbers/protocol-numbers.txt
udpProtoNum :: CInt
udpProtoNum = 17
