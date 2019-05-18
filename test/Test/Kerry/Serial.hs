{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Kerry.Serial (
    tests
  ) where

import qualified Data.Aeson as Aeson
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.IO.Class (liftIO)

import           Hedgehog
--import qualified Hedgehog.Gen as Gen
--import qualified Hedgehog.Range as Range

import           Kerry.Prelude
import           Kerry.Example (example)
import           Kerry.Serial (fromPacker, asByteStringWith)

import           System.Exit (ExitCode (..))
import qualified System.IO.Temp as Temp
--import qualified System.IO as IO
import qualified System.Process as Process

import qualified Data.ByteString as ByteString

prop_example :: Property
prop_example =
  property . hoist runResourceT $ do
    (_release, path, handle) <- Temp.openTempFile (Just "/tmp/nick") "example.json"
--    let
--      path = "/tmp/nick/fred.json"
--    handle <- liftIO $ IO.openFile path IO.WriteMode
    let
      raw = asByteStringWith (fromPacker $ const Aeson.Null) example
    annotate $ show raw

    liftIO $ ByteString.hPut handle raw

    (ec, stdout, stderr) <-
      liftIO $ Process.readProcessWithExitCode "packer" ["validate", path] ""
    annotate stdout
    annotate stderr

    assert $ ec == ExitSuccess

tests :: IO Bool
tests =
  checkParallel $$(discover)
