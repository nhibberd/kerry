{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Kerry.Example (
    example
  ) where

import qualified Data.Map as Map

import           Kerry
import           Kerry.Builder.AmazonEC2 (AWS(..), Credentials(..))
import           Kerry.Builder.AmazonEC2 (SourceAmi(..))
import           Kerry.Builder.AmazonEC2 (blockDeviceMapping)
import           Kerry.Builder.AmazonEC2 (EBS(..))

import           Kerry.Provisioner.Shell (ShellType(..), shell)

import qualified Kerry.Engine as F

import           Kerry.Internal.Prelude

example :: Packer
example =
  Packer {
      variables = [
          UserVariable "name" "example-packer"
        ]
    , builders = [
          Builder (AmazonEBSBuilder $ aws builder) Nothing ssh
        ]
    , provisioners = [
          shellProvisioner
        ]
    , postProcessors = []
    }

ssh :: Communicator
ssh =
  SSH $ defaultSSHCommunicator "ec2-user"

aws :: a -> AWS a
aws b =
  AWS {
      awsRegion = "us-west-2"
    , awsCredentials = EnvironmentVariables
    , awsBuilder = b
    }

builder :: EBS
builder =
  EBS {
      ebsAmiName = "test"
    , ebsSourceAmi = SourceAmiId "ami-fred"
    , ebsInstanceType = "m4.xlarge"
    , ebsAmiDescription = Nothing
    , ebsAmiRegions = Nothing
    , ebsAmiUsers = Nothing
    , ebsAssociatePublicIpAddress = Just True
    , ebsAvailabilityZone = Nothing
    , ebsIAMInstanceProfile = Just "iam-fred"
    , ebsInsecureSkipTLSVerify = Nothing
    , ebsLaunchBlockDeviceMappings = [
          blockDeviceMapping "/dev/xvda" "gp2" 200 True
        ]
    , ebsRunTags = Map.fromList [
          ("name", F.renderRawTemplate $ F.user "name")
        ]
    , ebsSubnetId = Nothing
    , ebsTags = Map.fromList [
          ("created", F.renderRawTemplate F.timestamp)
        ]
    , ebsVpcId = Nothing
    }


shellProvisioner :: Provisioner
shellProvisioner =
  provisioner . ShellProvisioner . shell $
    Inline ["echo foo"]
