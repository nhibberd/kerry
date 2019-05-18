{-# LANGUAGE NoImplicitPrelude #-}
module Kerry.Command.Reverse (
    Reverse(..)
  , kerryReverse
  ) where

import qualified Data.List as List

import           Kerry.Reverse (reverse)
import           Kerry.Prelude

data Reverse =
  Reverse {
      reverseInput :: [String]
    , reverseSeparator :: String
    } deriving (Eq, Ord, Show)

kerryReverse :: Reverse -> ExceptT () IO ()
kerryReverse x =
  liftIO .
    putStrLn .
    List.intercalate (reverseSeparator x) $
    reverse (reverseInput x)
