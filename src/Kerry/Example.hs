{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Kerry.Example (
    example
  ) where

import qualified Data.Map as Map

import qualified Kerry.Functions as F
import           Kerry.Data
import           Kerry.Builder.AmazonEC2

import           Kerry.Prelude

example :: Packer
example =
  Packer {
      variables = []
    , builders = [
          Builder (AmazonEBSBuilder ebs) ssh
        ]
    , provisioners = []
    , postProcessors = []
    }

ssh :: Communicator
ssh =
  SSH $ defaultSSHCommunicator "ec2-user"

ebs :: EBS
ebs =
  EBS {
      ebsAmiName = "test"
    , ebsSourceAmi = SourceAmiId "ami-fred"
    , ebsInstanceType = "m4.xlarge"
    , ebsAccountId = Nothing
    , ebsRegion = "us-west-2"
    , ebsS3Bucket = Nothing
    , ebsLaunchBlockDeviceMappings = [
          blockDeviceMapping "/dev/xvda" "gp2" 200 True
        ]

    , ebsVpcId = ""
    , ebsSubnetId = ""
    , ebsAssociatePublicIpAddress = True
    , ebsIAMInstanceProfile = Just "iam-fred"
    , ebsInsecureSkipTLSVerify = Nothing
    , ebsRunTags = Map.fromList [
          ("name", F.renderRawTemplate $ F.user "name")
        ]
    , ebsTags = Map.fromList [
          ("created", F.renderRawTemplate F.timestamp)
        ]
    }
