{-# LANGUAGE NoImplicitPrelude #-}
module Kerry (
  -- * Core data types
    Packer(..)
  , renderPacker

  , UserVariable(..)

  , Builder(..)
  , BuilderType(..)
  , Communicator(..)
  , defaultSSHCommunicator

  , Provisioner(..)

  , PostProcessor(..)

  -- * Builders

  -- ** AmazonEC2
  , AWS(..)
  , Credentials(..)
  , SourceAmi(..)
  , AWSAmiOwner(..)
  , SourceAmiFilterKey(..)
  , BlockDeviceMapping(..)
  , blockDeviceMapping

  -- *** EBS Backed
  , EBS(..)
  , ebs

  ) where


import           Kerry.Data (Packer(..), renderPacker)
import           Kerry.Data (UserVariable(..))
import           Kerry.Data (Builder(..), BuilderType(..))
import           Kerry.Data (Communicator(..), defaultSSHCommunicator)
import           Kerry.Data (Provisioner(..))
import           Kerry.Data (PostProcessor(..))
import           Kerry.Builder.AmazonEC2 (AWS(..), Credentials(..))
import           Kerry.Builder.AmazonEC2 (SourceAmi(..), AWSAmiOwner(..), SourceAmiFilterKey(..))
import           Kerry.Builder.AmazonEC2 (BlockDeviceMapping(..), blockDeviceMapping)
import           Kerry.Builder.AmazonEC2 (EBS(..), ebs)
