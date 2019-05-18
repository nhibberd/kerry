{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Bench.Kerry.Reverse (
    benchmark
  ) where

import           Gauge.Main (Benchmark, bgroup, bench, whnf)

import           Kerry.Prelude
import           Kerry.Reverse (reverse)

reverse2 :: Int -> Int
reverse2 n =
  length .
  reverse .
  reverse $
  [1..n]

benchmark :: Benchmark
benchmark =
  bgroup "Bench.Kerry.Reverse" [
   bgroup "reverse2" [
        bench "1"  $ whnf reverse2 1
      , bench "5"  $ whnf reverse2 5
      , bench "9"  $ whnf reverse2 9
      , bench "11" $ whnf reverse2 11
      ]
    ]
