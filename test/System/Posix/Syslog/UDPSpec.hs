{-# LANGUAGE OverloadedStrings #-}

module System.Posix.Syslog.UDPSpec (spec) where

import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, readTime)

import System.Posix.Syslog.UDP
import Test.Hspec

spec :: Spec
spec = do
  describe "syslogPacket" $ do
    it "uses the NILVALUE for Nothing values" $
      let
        time = Nothing :: Maybe UTCTime
        result = syslogPacket [USER] [Debug] time Nothing Nothing Nothing Nothing Nothing "test log message"
        expected = "<15>1 - - - - - - test log message"
      in
        result `shouldBe` expected

    it "uses the NILVALUE for empty bytestring values" $
      let
        time = Nothing :: Maybe UTCTime
        result = syslogPacket [USER] [Debug] time (Just $ HostName "") (Just $ AppName "") (Just $ ProcessID "") (Just $ MessageID "") Nothing "test log message"
        expected = "<15>1 - - - - - - test log message"
      in
        result `shouldBe` expected

    it "uses the correct values otherwise" $
      let
        time = Nothing :: Maybe UTCTime
        result = syslogPacket [USER] [Debug] time (Just $ HostName "host_name") (Just $ AppName "app_name") (Just $ ProcessID "proc_id") (Just $ MessageID "msg_id") Nothing "test log message"
        expected = "<15>1 - host_name app_name proc_id msg_id - test log message"
      in
        result `shouldBe` expected

    it "correctly formats the time" $
      let
        time = readTime defaultTimeLocale "%FT%X%QZ" "2003-10-11T22:14:15.003Z" :: UTCTime
        result = syslogPacket [USER] [Debug] (Just time) Nothing Nothing Nothing Nothing Nothing "test log message"
        expected = "<15>1 2003-10-11T22:14:15.003Z - - - - - test log message"
      in
        result `shouldBe` expected
