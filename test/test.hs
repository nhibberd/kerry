{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Monad (when)

import           Kerry.Internal.Prelude

import           System.Exit (exitFailure)

import qualified Test.Kerry.Serial as Serial


main :: IO ()
main = do
  ok <- and <$> sequence [
      Serial.tests
    ]
  when (not ok) $
    exitFailure
