{-# LANGUAGE OverloadedStrings #-}

module Spec (main) where

import qualified System.Posix.Syslog.UDPSpec as UDPSpec
import System.Posix.Syslog.UDP
import Test.Hspec

main :: IO ()
main = do
    logMessages
    runSpecs

logMessages :: IO ()
logMessages = do
    syslog <- defaultConfig >>= initSyslogUdp
    syslog USER Debug "hello thar!"

runSpecs :: IO ()
runSpecs =
    hspec $ do
      describe "System.Posix.Syslog.UDP" UDPSpec.spec
