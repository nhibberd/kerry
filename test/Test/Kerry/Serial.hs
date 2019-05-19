{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Kerry.Serial (
    tests
  ) where

import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Char8 as Char8

import           Hedgehog

import           Kerry.Prelude
import           Kerry.Example (example)
import           Kerry.Data (fromPacker)
import           Kerry.Serial (asByteStringWith)

import           System.Exit (ExitCode (..))
import qualified System.IO.Temp as Temp
import qualified System.IO as IO
import qualified System.Process as Process

import qualified Test.Kerry.Gen as Gen


prop_example :: Property
prop_example =
  withTests 1 . property . hoist runResourceT $ do
    (_release, path, handle) <- Temp.openTempFile Nothing "example.json"
    let
      raw = asByteStringWith (fromPacker) example
    annotate $ show raw

    liftIO $ Char8.hPutStrLn handle raw
    liftIO $ IO.hClose handle

    annotate path
    (ec, stdout, stderr) <-
      liftIO $ Process.readProcessWithExitCode "packer" ["validate", path] ""
    annotate stdout
    annotate stderr

    assert $ ec == ExitSuccess

prop_gen :: Property
prop_gen =
  withTests 100 . property . hoist runResourceT $ do
    packer <- forAll Gen.genPacker
    (_release, path, handle) <- Temp.openTempFile Nothing "example.json"
    let
      raw = asByteStringWith (fromPacker) packer
    annotate $ show raw

    liftIO $ Char8.hPutStrLn handle raw
    liftIO $ IO.hClose handle

    annotate path
    (ec, stdout, stderr) <-
      liftIO $ Process.readProcessWithExitCode "packer" ["validate", path] ""
    annotate stdout
    annotate stderr

    assert $ ec == ExitSuccess



tests :: IO Bool
tests =
  checkParallel $$(discover)
