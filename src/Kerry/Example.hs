{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Kerry.Example (
    test
  ) where

import qualified Data.Map as Map

import qualified Kerry.Functions as F
import           Kerry.Data
import           Kerry.Builders.AmazonEC2

import           Kerry.Prelude

test :: Packer EBS
test =
  Packer {
      variables = []
    , builders = [
          Builder ebs ssh
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
      ebsAccountId = "test"
    , ebsAmiName = "test"
    , ebsInstanceType = "m4.xlarge"
    , ebsRegion = "us-west-2"
    , ebsSourceAmi = SourceAmiId "ami-fred"
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
