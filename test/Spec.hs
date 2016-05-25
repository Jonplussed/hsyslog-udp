{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Spec (main) where

import Test.Hspec (hspec)

import qualified System.Posix.Syslog.UDPSpec as UDPSpec

import System.Posix.Syslog.UDP

main :: IO ()
main = do
    logMessages
    runSpecs

logMessages :: IO ()
logMessages =
    withSyslog defaultConfig $ \syslog -> do
      syslog (MessageID "general") [USER] [Debug] "hello thar!"

runSpecs :: IO ()
runSpecs =
    hspec $ do
      describe "System.Posix.Syslog.UDP" UDPSpec.spec
