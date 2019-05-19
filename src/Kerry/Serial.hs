{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Kerry.Serial (

    fromPacker
  , fromBuilder
  , fromBuilderType
  , fromVariable
  , fromProvisioner
  , fromPostProcessor


  , list
  , (.=?)

  , asTextWith
  , asByteStringWith

  ) where


import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import           Data.Aeson (Value, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as A

import qualified Data.Map.Strict as Map

import           Kerry.Data
import           Kerry.Prelude

import qualified Kerry.Builder.AmazonEC2 as AmazonEC2


-- This type variable is wrong
fromPacker :: Packer -> Value
fromPacker p =
  A.object $ join [
      "variables" .=? (listToObject <$> list (fromVariable <$> variables p))
    , "builders" .=? list (fromBuilder <$> builders p)
    , "provisioners" .=? list (fromProvisioner <$> provisioners p)
    , "post-processors" .=? list (fromPostProcessor <$> postProcessors p)
    ]

fromBuilder :: Builder -> Value
fromBuilder (Builder btype comm) =
  A.object $ join [
      fromBuilderType btype
    , fromCommunicator comm
    ]

fromBuilderType :: BuilderType -> [A.Pair]
fromBuilderType = \case
  AmazonEBSBuilder ebs ->
    -- TODO this should live somewhere else
    "type" .= t "amazon-ebs" : [
      "ami_name" .= AmazonEC2.ebsAmiName ebs
    , case AmazonEC2.ebsSourceAmi ebs of
        AmazonEC2.SourceAmiId x ->
          "source_ami" .= x
    , "instance_type" .= AmazonEC2.ebsInstanceType ebs
    ]

fromCommunicator :: Communicator -> [A.Pair]
fromCommunicator = \case
  None ->
    []
  SSH (SSHCommunicator user pty timeout) -> [
      "ssh_username" .= user
    , "ssh_pty" .= pty
    , "ssh_timeout" .= (show timeout <> "m")
    ]
  WinRm ->
    []


fromVariable :: Variable -> A.Pair
fromVariable (Variable k v) =
  k .= v

fromProvisioner :: Provisioner -> Value
fromProvisioner p =
  A.object $ join [
      ["type" .= provisionerType p]
    , Map.foldrWithKey (\k v xs -> (k .= v : xs)) [] (provisionerOptions p)
    , "only" .=? list (provisionerOnly p)
    , "except" .=? list (provisionerExcept p)
    , "pause_before" .=? (provisionerPauseBefore p)
    , "timeout" .=? (provisionerTimeout p)
--    , "override" .=?
    ]

fromPostProcessor :: PostProcessor -> Value
fromPostProcessor _ =
  A.object $ join [
    ]

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
