{-# LANGUAGE OverloadedStrings #-}

 module System.Posix.Syslog.UDP where
  ( -- * Marshaled Data Types
    Priority (..)
  , toPriority
  , fromPriority
  , Facility (..)
  , toFacility
  , fromFacility
  , Option (..)
  , toOption
  , fromOption
  , PriorityMask (..)
  , fromPriorityMask
    -- * Configuring syslog
  , SyslogConfig (..)
    -- * The preferred Haskell API to syslog
  , withSyslog
  , SyslogFn
    -- * The unsafe Haskell API to syslog
  , syslogUnsafe
  ) where

import Data.ByteString (ByteString, pack)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Network.Socket.ByteString ()
import System.Posix.Types (CPid (..))

import qualified Network.Socket as S
import qualified Network.HostName as H

import System.Posix.Syslog.FFI

newtype AppName = AppName ByteString
newtype HostName = HostName ByteString
newtype MessageID = MessageID ByteString

data SyslogConfig = SyslogConfig
  { udpSocketAddress  :: S.SockAddr
  , identifier        :: ByteString   -- ^ string appended to each log message
  } deriving (Eq, Show)

withSyslog :: SyslogConfig -> (SyslogFn -> IO ()) -> IO ()
withSyslog config f = do
    sock <- S.socket (familyFromAddr addr) S.Datagram S.defaultProtocol
    host <- getHostName
    proc <- getProcessID
    bracket_ (S.connect sock addr) (S.close sock) (f logger)
  where
    addr = udpSocketAddress config

syslogUnsafe :: S.Socket -> ByteString -> [Facility] -> [Priority] -> ByteString -> IO ()
syslogUnsafe socket ident facs pris msg = do
    host <- getHostName
    time <- getCurrentTime
    let packet = makePacket ident time

logger pris facs msg = do

-- internal functions

familyFromAddr :: S.SockAddr -> S.Family
familyFromAddr (S.SockAddrInet _ _) = S.AF_INET
familyFromAddr (S.SockAddrInet6 _ _ _ _) = S.AF_INET6
familyFromAddr (S.SockAddrUnix _) = S.AF_UNIX

-- | Dictated by <https://tools.ietf.org/html/rfc5424 RFC 5424>.

makePacket
  :: HostName
  -> CPid
  -> UTCTime
  -> AppName
  -> MessageID
  -> [Facility]
  -> [Priority]
  -> ByteString
  -> ByteString
makePacket host cpid appName msgId time facs pris msg =
         prival
     <>  version
    `sp` timestamp
    `sp` hostName
    `sp` appName
    `sp` processId
  where
    t1 `sp` t2 = t1 <> " " <> t2
    prival = "<" <> pack (show $ makePri facs pris) <> ">"
    version = "1"
    timestamp = pack $ formatTime defaultTimeLocale "%FT%X%QZ" time
    hostName = pack host
    processId = 


