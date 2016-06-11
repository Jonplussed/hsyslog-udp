{-# LANGUAGE OverloadedStrings #-}

module System.Posix.Syslog.UDPSpec (spec) where

import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

import System.Posix.Syslog.UDP
import Test.Hspec

spec :: Spec
spec = do
  describe "maskedPriVal" $ do
    it "returns a Just if priorities remain after masking" $
      let result = maskedPriVal (UpTo Debug) LOCAL4 Notice
      in result `shouldBe` Just (PriVal 165)

    it "returns Nothing if all priorities are masked" $
      let result = maskedPriVal (UpTo Info) USER Debug
      in result `shouldBe` Nothing

  describe "rfc5424packet" $ do
    let (Just priVal) = maskedPriVal NoMask USER Debug

    it "uses the NILVALUE for Nothing values" $
      let
        time = Nothing :: Maybe UTCTime
        result = rfc5424packet priVal time Nothing Nothing Nothing Nothing Nothing "test log message"
        expected = "<15>1 - - - - - - test log message"
      in
        result `shouldBe` expected

    it "uses the NILVALUE for empty bytestring values" $
      let
        time = Nothing :: Maybe UTCTime
        result = rfc5424packet priVal time (Just $ HostName "") (Just $ AppName "") (Just $ ProcessID "") (Just $ MessageID "") Nothing "test log message"
        expected = "<15>1 - - - - - - test log message"
      in
        result `shouldBe` expected

    it "uses the correct values otherwise" $
      let
        time = Nothing :: Maybe UTCTime
        result = rfc5424packet priVal time (Just $ HostName "host_name") (Just $ AppName "app_name") (Just $ ProcessID "proc_id") (Just $ MessageID "msg_id") Nothing "test log message"
        expected = "<15>1 - host_name app_name proc_id msg_id - test log message"
      in
        result `shouldBe` expected

    it "correctly formats the time" $
      let
        time = parseTimeM True defaultTimeLocale "%FT%X%QZ" "2003-10-11T22:14:15.003Z" :: Maybe UTCTime
        result = rfc5424packet priVal time Nothing Nothing Nothing Nothing Nothing "test log message"
        expected = "<15>1 2003-10-11T22:14:15.003Z - - - - - test log message"
      in
        result `shouldBe` expected

  describe "rfc3164packet" $ do
    let
      (Just priVal) = maskedPriVal NoMask USER Debug
      (Just time) = parseTimeM True defaultTimeLocale "%FT%X%QZ" "2003-10-11T22:14:15.003Z" :: Maybe UTCTime
      host = (HostName "host_name")

    it "correctly formats the message" $
      let
        result = rfc3164packet priVal time host "test log message"
        expected = "<15>Oct 11 22:14:15 host_name test log message"
      in
        result `shouldBe` expected
