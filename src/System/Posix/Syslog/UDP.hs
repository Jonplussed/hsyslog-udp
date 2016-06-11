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
  , rfc5424protocol
  , rfc3164protocol
    -- * Manually constructing syslog UDP packets
  , rfc5424packet
  , rfc3164packet
    -- ** Miscellaneous utilities
  , resolveUdpAddress
  , getAppName
  , getHostName
  , getProcessId
  , maskedPriVal
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

newtype AppName
  = AppName ByteString
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.5 APP-NAME>@
  deriving (Eq, Show)

newtype HostName
  = HostName ByteString
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.4 HOSTNAME>@;
  -- fetch via 'getHostName'
  deriving (Eq, Show)

newtype PriVal
  = PriVal CInt
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.1 PRI>@;
  -- construct via 'maskedPriVal'
  deriving (Eq, Show)

newtype ProcessID
  = ProcessID ByteString
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.6 PROCID>@;
  -- fetch via 'getProcessId'
  deriving (Eq, Show)

newtype MessageID
  = MessageID ByteString
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.7 MSGID>@
  deriving (Eq, Show)

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

-- | The type of function returned by 'withSyslog'.

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
    -- ^ protocol for formatting the message, such as 'rfc5424protocol' or
    -- 'rfc3164protocol'
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
    protocol = rfc3164protocol

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

rfc5424packet
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
rfc5424packet priVal time hostName' appName' processId' messageId _ message =
         formatPriVal priVal
     <>  version
    `sp` orNil mkTime time
    `sp` orNil mkHost hostName'
    `sp` orNil mkApp appName'
    `sp` orNil mkProcId processId'
    `sp` orNil mkMsgId messageId
    `sp` structData
    `sp` T.encodeUtf8 message
  where
    formatPriVal (PriVal x) = "<" <> B.pack (show x) <> ">"
    version = "1"
    mkTime = B.pack . formatTime defaultTimeLocale "%FT%X%QZ"
    mkHost (HostName x) = notEmpty x
    mkApp (AppName x) = notEmpty x
    mkProcId (ProcessID x) = notEmpty x
    mkMsgId (MessageID x) = notEmpty x
    structData = nilValue

rfc5424protocol :: Protocol
rfc5424protocol priVal time hostName' appName' processId' message =
    rfc5424packet priVal (Just time) (Just hostName') (Just appName')
      (Just processId') Nothing Nothing message

-- | Construct a syslog UDP packet as dictated by
-- <https://tools.ietf.org/html/rfc3164 RFC 3164>. Note that fields in a syslog
-- packet are whitespace-delineated, so don't allow whitespace in anything but
-- the log message!

rfc3164packet
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
  -> Text
  -- ^ see @<https://tools.ietf.org/html/rfc3164#section-4.1.3 MSG>@
  -> ByteString
rfc3164packet priVal time hostName' message =
         formatPriVal priVal
     <>  mkTime time
    `sp` mkHost hostName'
    `sp` T.encodeUtf8 message
  where
    formatPriVal (PriVal x) = "<" <> B.pack (show x) <> ">"
    mkTime = B.pack . formatTime defaultTimeLocale "%b %e %X"
    mkHost (HostName x) = notEmpty x

rfc3164protocol :: Protocol
rfc3164protocol priVal time hostName' _ _ message =
    rfc3164packet priVal time hostName' message

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

nilValue :: ByteString
nilValue = "-"

notEmpty :: ByteString -> ByteString
notEmpty bs = if B.null bs then nilValue else bs

orNil :: (a -> ByteString) -> Maybe a -> ByteString
orNil = maybe nilValue

safely :: IO a -> IO ()
safely f = catch (void f) (const $ return () :: SomeException -> IO ())

sp :: ByteString -> ByteString -> ByteString
sp b1 b2 = b1 <> " " <> b2
{-# INLINE sp #-}

-- see http://www.iana.org/assignments/protocol-numbers/protocol-numbers.txt
udpProtoNum :: CInt
udpProtoNum = 17
