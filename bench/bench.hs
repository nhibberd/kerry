{-# LANGUAGE NoImplicitPrelude #-}

import qualified Bench.Kerry.Reverse

import           Gauge.Main (defaultMain)

import           Kerry.Internal.Prelude


main :: IO ()
main =
  defaultMain [
      Bench.Kerry.Reverse.benchmark
    ]
