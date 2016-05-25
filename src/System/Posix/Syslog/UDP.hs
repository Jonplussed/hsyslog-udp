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
    -- ** Universal to syslog
    L.Priority (..)
  , L.Facility (..)
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
    -- * Manually constructing syslog UDP packets
  , syslogPacket
  ) where

import Control.Exception (bracket_)
import Data.Bits (Bits, (.|.))
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Foreign.C (CInt)
import Network.Socket.ByteString (send)
import System.Posix.Types (CPid (..))

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T
import qualified Network.Socket as S
import qualified Network.HostName as H
import qualified System.Posix.Process as P
import qualified System.Posix.Syslog as L

newtype AppName = AppName ByteString deriving (Eq, Show)
newtype HostName = HostName ByteString deriving (Eq, Show)
newtype ProcessID = ProcessID ByteString deriving (Eq, Show)
newtype MessageID = MessageID ByteString deriving (Eq, Show)

data StructuredData = StructuredData

-- | Wrap an IO computation with the ability to log to syslog via UDP. This
-- handles opening and closing the syslog socket and provides much of the
-- information desired in the syslog packet.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import System.Posix.Syslog.UDP
-- >
-- > main = withSyslog mySyslogConfig $
-- >   \syslog -> do
-- >     putStrLn "huhu"
-- >     syslog (MessageID "general") [USER] [Debug] "huhu"

withSyslog :: SyslogConfig -> (SyslogFn -> IO ()) -> IO ()
withSyslog config f = do
    socket <- S.socket (familyFromAddr address) S.Datagram S.defaultProtocol
    hostName <- getHostName
    processId <- getProcessId

    bracket_ (S.connect socket address) (S.close socket) $
      f $ \messageId facilities priorities message -> do
        time <- getCurrentTime
        _ <- send socket $ syslogPacket facilities priorities (Just time)
          (Just hostName) (Just $ appName config) (Just processId)
          (Just messageId) Nothing message
        return ()
  where
    address = udpSockAddr config

-- | Configuration options for connecting and logging to your syslog socket.

data SyslogConfig = SyslogConfig
  { udpSockAddr :: S.SockAddr -- ^ where to send the syslog packets
  , appName     :: AppName    -- ^ string appended to each log message
  } deriving (Eq, Show)

-- | A convenient default config for local use, connecting on @127.0.0.1:514@.

defaultConfig :: SyslogConfig
defaultConfig =
    SyslogConfig
      { udpSockAddr = S.SockAddrInet 514 2130706433
      , appName = AppName "hsyslog-udp"
      }

-- | The type of function provided by 'withSyslog'.

type SyslogFn
  =  MessageID    -- ^ arbitrary message classifier
  -> [L.Facility] -- ^ facilities to log to
  -> [L.Priority] -- ^ severities under which to log
  -> Text         -- ^ message body
  -> IO ()

-- | If 'withSyslog' is too rigid for your application's constraints,
-- 'syslogPacket' at least provides you the ability to create syslog UDP
-- packets as dictated by <https://tools.ietf.org/html/rfc5424 RFC 5424>.

syslogPacket
  :: FormatTime t
  => [L.Facility]         -- ^ facilities in @<https://tools.ietf.org/html/rfc5424#section-6.2.1 PRI>@
  -> [L.Priority]         -- ^ severities in @<https://tools.ietf.org/html/rfc5424#section-6.2.1 PRI>@
  -> Maybe t              -- ^ time of message, converted to @<https://tools.ietf.org/html/rfc5424#section-6.2.3 TIMESTAMP>@
  -> Maybe HostName       -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.4 HOSTNAME>@
  -> Maybe AppName        -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.5 APP-NAME>@
  -> Maybe ProcessID      -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.6 PROCID>@
  -> Maybe MessageID      -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.2.7 MSGID>@
  -> Maybe StructuredData -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.3 STRUCTURED-DATA>@ (unsupported)
  -> Text                 -- ^ see @<https://tools.ietf.org/html/rfc5424#section-6.4 MSG>@
  -> ByteString
syslogPacket facilities priorities time hostName appName' processId messageId _
  message =
         prival
    <>   version
    `sp` orNil mkTime time
    `sp` orNil mkHost hostName
    `sp` orNil mkApp appName'
    `sp` orNil mkProcId processId
    `sp` orNil mkMsgId messageId
    `sp` structData
    `sp` T.encodeUtf8 message
  where
    prival = "<" <> B.pack (show $ mkPriVal facilities priorities) <> ">"
    version = "1"
    mkTime = B.pack . formatTime defaultTimeLocale "%FT%X%QZ"
    mkHost (HostName x) = notEmpty x
    mkApp (AppName x) = notEmpty x
    mkProcId (ProcessID x) = notEmpty x
    mkMsgId (MessageID x) = notEmpty x
    structData = nilValue

-- internal functions

bitsOrWith :: (Bits b, Num b) => (a -> b) -> [a] -> b
bitsOrWith f = foldl' (\bits x -> f x .|. bits) 0

familyFromAddr :: S.SockAddr -> S.Family
familyFromAddr (S.SockAddrInet _ _) = S.AF_INET
familyFromAddr (S.SockAddrInet6 _ _ _ _) = S.AF_INET6
familyFromAddr (S.SockAddrUnix _) = S.AF_UNIX
familyFromAddr (S.SockAddrCan _) = S.AF_CAN

getHostName :: IO HostName
getHostName = HostName . B.pack <$> H.getHostName

getProcessId :: IO ProcessID
getProcessId = do
    (CPid pid) <- P.getProcessID
    return . ProcessID . B.pack $ show pid

mkPriVal :: [L.Facility] -> [L.Priority] -> CInt
mkPriVal facs pris =
    L._LOG_MAKEPRI
      (bitsOrWith L.fromFacility facs)
      (bitsOrWith L.fromPriority pris)

nilValue :: ByteString
nilValue = "-"

notEmpty :: ByteString -> ByteString
notEmpty bs = if B.null bs then nilValue else bs

orNil :: (a -> ByteString) -> Maybe a -> ByteString
orNil = maybe nilValue

sp :: ByteString -> ByteString -> ByteString
sp b1 b2 = b1 <> " " <> b2
{-# INLINE sp #-}
