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
    -- ** Newtypes for various string identifiers
    -- | Refer to
    -- <https://tools.ietf.org/html/rfc5424#section-6.2 RFC 5424 section 6.2>
    -- as to the purpose of each.
  , AppName (..)
  , HostName (..)
  , ProcessID (..)
  , MessageID (..)
  -- ** Structured Data
  -- | Currently unsupported; a placeholder for future use.
  , StructuredData (..)
    -- * The easy Haskell API to syslog via UDP
  , withSyslog
  , SyslogFn
  , SyslogConfig (..)
  , defaultConfig
  , S.SockAddr (..)
    -- * Manually constructing and sending syslog UDP packets
  , udpSocketForAddr
  , maskedPriVal
  , syslogPacket
    -- ** Miscellaneous utilities
  , getHostName
  , getProcessId
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.Bits (Bits, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Foreign.C (CInt)
import System.Posix.Types (CPid (..))

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import qualified Network.HostName as H
import qualified System.Posix.Process as P
import qualified System.Posix.Syslog as L

newtype AppName = AppName ByteString deriving (Eq, Show)
newtype HostName = HostName ByteString deriving (Eq, Show)
newtype ProcessID = ProcessID ByteString deriving (Eq, Show)
newtype MessageID = MessageID ByteString deriving (Eq, Show)

data StructuredData = StructuredData

-- | Wrap an IO computation with the ability to log to syslog via UDP.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import System.Posix.Syslog.UDP
-- >
-- > main = withSyslog mySyslogConfig $
-- >   \syslog -> do
-- >     putStrLn "huhu"
-- >     syslog (MessageID "general") [USER] [Debug] "huhu"
--
-- This makes no assumptions about socket connection status or endpoint
-- availability. Any errors while sending are silently ignored.

withSyslog :: SyslogConfig -> (SyslogFn -> IO ()) -> IO ()
withSyslog config f = do
    socket <- udpSocketForAddr $ udpSockAddr config
    hostName <- getHostName
    processId <- getProcessId

    let send bytes = catch
          (void . SB.sendTo socket bytes $ udpSockAddr config)
          (const $ return () :: SomeException -> IO ())

    f $ \messageId facilities priorities message ->
      case maskedPriVal (priorityMask config) facilities priorities of
        Nothing -> return ()
        Just priVal -> do
          time <- getCurrentTime
          send $ syslogPacket priVal (Just time) (Just hostName)
            (Just $ appName config) (Just processId) (Just messageId) Nothing
            message

-- | Configuration options for connecting and logging to your syslog socket.

data SyslogConfig = SyslogConfig
  { appName :: AppName
    -- ^ string appended to each log message
  , priorityMask :: L.PriorityMask
    -- ^ whitelist of priorities of logs to send
  , udpSockAddr :: S.SockAddr
    -- ^ where to send the syslog packets
  } deriving (Eq, Show)

-- | A convenient default config for local use, connecting on @127.0.0.1:514@.

defaultConfig :: SyslogConfig
defaultConfig =
    SyslogConfig
      { appName = AppName "hsyslog-udp"
      , priorityMask = L.NoMask
      , udpSockAddr = S.SockAddrInet 514 16777343
      }

-- | The type of function provided by 'withSyslog'.

type SyslogFn
  =  MessageID    -- ^ arbitrary message classifier
  -> [L.Facility] -- ^ facilities to log to
  -> [L.Priority] -- ^ severities under which to log
  -> Text         -- ^ message body
  -> IO ()

-- | Initialize a datagram socket appropriate for your endpoint's address

udpSocketForAddr :: S.SockAddr -> IO S.Socket
udpSocketForAddr addr =
    S.socket (familyFromAddr addr) S.Datagram S.defaultProtocol

-- | Construct a @<https://tools.ietf.org/html/rfc5424#section-6.2.1 PRI>@.
-- 'Nothing' indicates that the priorities are fully masked, and so no packet
-- should be sent.

maskedPriVal
  :: L.PriorityMask
  -> [L.Facility]
  -> [L.Priority]
  -> Maybe CInt
-- this switching is required because the CInt representation of NoMask is 0
maskedPriVal _ _ [] = Nothing
maskedPriVal mask facs pris =
    case mask of
      L.NoMask -> Just $ L._LOG_MAKEPRI facVal priVal
      _ | maskedPriVal == 0 -> Nothing
      _ -> Just $ L._LOG_MAKEPRI facVal maskedPriVal
  where
    facVal = bitsOrWith L.fromFacility facs
    priVal = bitsOrWith L.fromPriority pris
    maskedPriVal = L.fromPriorityMask mask .&. priVal

-- | Construct a syslog UDP packet as dictated by
-- <https://tools.ietf.org/html/rfc5424 RFC 5424>.

syslogPacket
  :: FormatTime t
  => CInt                 -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.1 PRI>@; construct via 'maskedPriVal'
  -> Maybe t              -- ^ time of message, converted to @<https://tools.ietf.org/html/rfc5424#section-6.2.3 TIMESTAMP>@
  -> Maybe HostName       -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.4 HOSTNAME>@; fetch via 'getHostName'
  -> Maybe AppName        -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.5 APP-NAME>@
  -> Maybe ProcessID      -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.6 PROCID>@; fetch via 'getProcessId'
  -> Maybe MessageID      -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.7 MSGID>@
  -> Maybe StructuredData -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.3 STRUCTURED-DATA>@ (unsupported)
  -> Text                 -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.4 MSG>@
  -> ByteString
syslogPacket priVal time hostName appName' processId messageId _ message =
         formatPriVal
    <>   version
    `sp` orNil mkTime time
    `sp` orNil mkHost hostName
    `sp` orNil mkApp appName'
    `sp` orNil mkProcId processId
    `sp` orNil mkMsgId messageId
    `sp` structData
    `sp` T.encodeUtf8 message
  where
    formatPriVal = "<" <> B.pack (show priVal) <> ">"
    version = "1"
    mkTime = B.pack . formatTime defaultTimeLocale "%FT%X%QZ"
    mkHost (HostName x) = notEmpty x
    mkApp (AppName x) = notEmpty x
    mkProcId (ProcessID x) = notEmpty x
    mkMsgId (MessageID x) = notEmpty x
    structData = nilValue

getHostName :: IO HostName
getHostName = HostName . B.pack <$> H.getHostName

getProcessId :: IO ProcessID
getProcessId = do
    (CPid pid) <- P.getProcessID
    return . ProcessID . B.pack $ show pid

-- internal functions

bitsOrWith :: (Bits b, Num b) => (a -> b) -> [a] -> b
bitsOrWith f = foldl' (\bits x -> f x .|. bits) 0

familyFromAddr :: S.SockAddr -> S.Family
familyFromAddr addr = case addr of
    S.SockAddrInet {}   -> S.AF_INET
    S.SockAddrInet6 {}  -> S.AF_INET6
    S.SockAddrUnix {}   -> S.AF_UNIX
    S.SockAddrCan {}    -> S.AF_CAN

nilValue :: ByteString
nilValue = "-"

notEmpty :: ByteString -> ByteString
notEmpty bs = if B.null bs then nilValue else bs

orNil :: (a -> ByteString) -> Maybe a -> ByteString
orNil = maybe nilValue

sp :: ByteString -> ByteString -> ByteString
sp b1 b2 = b1 <> " " <> b2
{-# INLINE sp #-}
