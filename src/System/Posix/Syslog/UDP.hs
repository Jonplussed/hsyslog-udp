{-# LANGUAGE OverloadedStrings #-}

module System.Posix.Syslog.UDP
  ( -- * Marshaled Data Types
    L.Priority (..)
  , L.toPriority
  , L.fromPriority
  , L.Facility (..)
  , L.toFacility
  , L.fromFacility
    -- * Configuring syslog
  , SyslogConfig (..)
    -- * The easy Haskell API to syslog via UDP
  , withSyslog
  , SyslogFn
    -- * Constructing a syslog UDP packet
  , syslogPacket
  ) where

import Control.Exception (bracket_)
import Data.Bits (Bits, (.|.))
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Foreign.C (CInt)
import Network.Socket.ByteString (send)
import System.Posix.Process (getProcessID)
import System.Posix.Types (CPid (..))

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T
import qualified Network.Socket as S
import qualified Network.HostName as H
import qualified System.Posix.Syslog as L

newtype AppName = AppName ByteString deriving (Eq, Show)
newtype HostName = HostName ByteString deriving (Eq, Show)
newtype MessageID = MessageID ByteString deriving (Eq, Show)

data StructuredData = StructuredData -- currently unsupported

data SyslogConfig = SyslogConfig
  { udpSockAddr :: S.SockAddr
  , appName     :: ByteString   -- ^ string appended to each log message
  } deriving (Eq, Show)

withSyslog :: SyslogConfig -> (SyslogFn -> IO ()) -> IO ()
withSyslog config f = do
    sock <- S.socket (familyFromAddr addr) S.Datagram S.defaultProtocol
    host <- getHostName
    proc <- getProcessID

    bracket_ (S.connect sock addr) (S.close sock) $
      f $ \mid facs pris msg -> do
        now <- getCurrentTime
        send sock $ syslogPacket facs pris (Just now) (Just host) (Just app)
          (Just proc) (Just mid) Nothing msg
        return ()
  where
    app = AppName $ appName config
    addr = udpSockAddr config

type SyslogFn
  =  MessageID
  -> [L.Facility]
  -> [L.Priority]
  -> Text
  -> IO ()

-- | Dictated by <https://tools.ietf.org/html/rfc5424 RFC 5424>.

nilValue :: ByteString
nilValue = "-"

syslogPacket
  :: [L.Facility]
  -> [L.Priority]
  -> Maybe UTCTime
  -> Maybe HostName
  -> Maybe AppName
  -> Maybe CPid
  -> Maybe MessageID
  -> Maybe StructuredData
  -> Text
  -> ByteString
syslogPacket facs pris time host app pid mid _ msg =
         prival             -- 6.2.1 PRI
    <>   version            -- 6.2.2 VERSION
    `sp` orNil mkTime time  -- 6.2.3 TIMESTAMP
    `sp` orNil mkHost host  -- 6.2.4 HOSTNAME
    `sp` orNil mkApp app    -- 6.2.5 APP-NAME
    `sp` orNil mkProcId pid -- 6.2.6 PROCID
    `sp` orNil mkMsgId mid  -- 6.2.7 MSGID
    `sp` structData         -- 6.3
    `sp` T.encodeUtf8 msg   -- 6.4   MSG
  where
    prival = "<" <> B.pack (show $ mkPriVal facs pris) <> ">"
    version = "1"
    mkTime = B.pack . formatTime defaultTimeLocale "%FT%X%QZ"
    mkHost (HostName x) = notEmpty x
    mkApp (AppName x) = notEmpty x
    mkProcId (CPid x) = B.pack $ show x
    mkMsgId (MessageID x) = notEmpty x
    structData = nilValue

-- internal functions

bitsOrWith :: (Bits b, Num b) => (a -> b) -> [a] -> b
bitsOrWith f = foldl' (\bits x -> f x .|. bits) 0

familyFromAddr :: S.SockAddr -> S.Family
familyFromAddr (S.SockAddrInet _ _) = S.AF_INET
familyFromAddr (S.SockAddrInet6 _ _ _ _) = S.AF_INET6
familyFromAddr (S.SockAddrUnix _) = S.AF_UNIX

getHostName :: IO HostName
getHostName = HostName . B.pack <$> H.getHostName

mkPriVal :: [L.Facility] -> [L.Priority] -> CInt
mkPriVal facs pris =
    L._LOG_MAKEPRI
      (bitsOrWith L.fromFacility facs)
      (bitsOrWith L.fromPriority pris)

notEmpty :: ByteString -> ByteString
notEmpty bs = if B.null bs then nilValue else bs

orNil :: (a -> ByteString) -> Maybe a -> ByteString
orNil = maybe nilValue

sp :: ByteString -> ByteString -> ByteString
sp b1 b2 = b1 <> " " <> b2
{-# INLINE sp #-}
