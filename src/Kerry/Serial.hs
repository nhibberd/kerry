{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Kerry.Serial (
    list
  , listToObject
  , t
  , (.=?)

  , asTextWith
  , asByteStringWith

  , prettyAsTextWith
  , prettyAsByteStringWith

  ) where

import           Data.Aeson (Value, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.Aeson.Types as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import           Kerry.Prelude

listToObject :: [A.Pair] -> Value
listToObject xs =
  A.object . flip fmap xs $ \x ->
    x

list :: [a] -> Maybe [a]
list l =
  if null l then
    Nothing
  else
    Just l

(.=?) :: A.ToJSON a => Text -> Maybe a -> [(Text, Value)]
(.=?) k =
  maybe [] (\x -> [k .= x])

t :: Text -> Text
t =
  id

asTextWith :: (a -> Value) -> a -> Text
asTextWith from =
  T.decodeUtf8 . asByteStringWith from

asByteStringWith :: (a -> Value) -> a -> ByteString
asByteStringWith from =
  Lazy.toStrict . Aeson.encode . from

prettyAsTextWith :: (a -> Value) -> a -> Text
prettyAsTextWith from =
  T.decodeUtf8 . asByteStringWith from

prettyAsByteStringWith :: (a -> Value) -> a -> ByteString
prettyAsByteStringWith from =
  Lazy.toStrict . Pretty.encodePretty . from
