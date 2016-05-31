{-# LANGUAGE OverloadedStrings #-}

{- |
  A convenient interface for logging to syslog via UDP.

  The following features are currently missing (but may be provided in future
  versions):

    * validation of string identifiers such as @APP-NAME@ or @MSGID@
    * support for @STRUCTURED-DATA@
    * verification that a UDP packet was fully transmitted
-}

module System.Posix.Syslog.UDP
  ( -- * Syslog UDP packet component datatypes
    -- ** Re-exports from hsyslog
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
  , initSyslogUdp
  , SyslogFn
  , SyslogConfig (..)
  , defaultConfig
  , localhost
    -- * Manually constructing and sending syslog UDP packets
  , syslogPacket
    -- ** Miscellaneous utilities
  , getHostName
  , getProcessId
  , getSyslogOnHost
  , maskedPriVal
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Foreign.C (CInt)
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
-- >   syslog <- initSyslogUdp defaultConfig
-- >   putStrLn "huhu"
-- >   syslog USER Debug "huhu"
--
-- This makes no assumptions about socket connection status or endpoint
-- availability. Any errors while sending are silently ignored.

initSyslogUdp :: SyslogConfig -> IO SyslogFn
initSyslogUdp config = S.withSocketsDo $ do
    socket <- S.socket (S.addrFamily $ address config) S.Datagram udpProtoNum
    hostName <- getHostName
    processId <- getProcessId
    let send = flip (SB.sendTo socket) (S.addrAddress $ address config)

    return $ \facility severity message ->
      case maskedPriVal (severityMask config) facility severity of
        Nothing -> return ()
        Just priVal -> do
          time <- getCurrentTime
          safely . send $ syslogPacket priVal (Just time) (Just hostName)
            (Just $ appName config) (Just processId) Nothing Nothing message

-- | The type of function returned by 'withSyslog'.

type SyslogFn
  =  L.Facility -- ^ facility to log to
  -> Severity   -- ^ severity under which to log
  -> Text       -- ^ message body
  -> IO ()

-- | Configuration options for connecting and logging to your syslog socket.

data SyslogConfig = SyslogConfig
  { appName :: AppName
    -- ^ string appended to each log message
  , severityMask :: SeverityMask
    -- ^ whitelist of priorities of logs to send
  , address :: S.AddrInfo
    -- ^ where to send the syslog packets; find via 'getSyslogOnHost'
    -- or 'S.getAddrInfo'
  } deriving (Eq, Show)

-- | A convenient default config, connecting to
-- 'localhost'. Provided for development/testing purposes.

defaultConfig :: SyslogConfig
defaultConfig =
    SyslogConfig
      { appName = AppName "hsyslog-udp"
      , severityMask = L.NoMask
      , address = localhost
      }

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

syslogPacket
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
  -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.5 APP-NAME>@
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
syslogPacket priVal time hostName appName' processId messageId _ message =
         formatPriVal priVal
    <>   version
    `sp` orNil mkTime time
    `sp` orNil mkHost hostName
    `sp` orNil mkApp appName'
    `sp` orNil mkProcId processId
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

getHostName :: IO HostName
getHostName = HostName . B.pack <$> BSD.getHostName

getProcessId :: IO ProcessID
getProcessId = do
    (CPid pid) <- P.getProcessID
    return . ProcessID . B.pack $ show pid

-- | Return any syslog/UDP identified endpoints at the given hostname or IP
-- address. You'll have to select from the results.

getSyslogOnHost :: String -> IO [S.AddrInfo]
getSyslogOnHost hostname =
    S.getAddrInfo (Just hints) (Just hostname) (Just "syslog")
  where
    hints = S.defaultHints
        { S.addrSocketType = S.Datagram
        , S.addrProtocol = udpProtoNum
        }

-- | Construct a @<https://tools.ietf.org/html/rfc5424#section-6.2.1 PRI>@.
-- 'Nothing' indicates that the severities are fully masked, and so no packet
-- should be sent.

maskedPriVal
  :: SeverityMask
  -> L.Facility
  -> Severity
  -> Maybe PriVal
-- switching required because the CInt representation of NoMask is 0
maskedPriVal mask fac sev
    | mask == L.NoMask  = prival
    | remaining == 0    = Nothing
    | otherwise         = prival
  where
    prival = Just . PriVal $ L.makePri fac sev
    remaining = L._LOG_MASK (L.fromPriority sev) .&. L.fromPriorityMask mask

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
