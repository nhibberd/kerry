{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Monad (when)

import           Kerry.Prelude

import           System.Exit (exitFailure)

import qualified Test.Kerry.Reverse


main :: IO ()
main = do
  ok <- and <$> sequence [
      Test.Kerry.Reverse.tests
    ]
  when (not ok) $
    exitFailure
