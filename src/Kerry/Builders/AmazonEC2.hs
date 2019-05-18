{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Kerry.Builders.AmazonEC2 (
  -- * General
    Credentials (..)
  , AWSVariables (..)

  -- * Util
  , AWSAmiOwner (..)
  , SourceAmi (..)
  , SourceAmiFilterKey (..)
  , BlockDeviceMapping (..)
  , blockDeviceMapping

  -- * Builders
  -- ** EBS Backed
  , EBS (..)

  -- * Instance-store
  , Instance (..)

  ) where

import           Kerry.Prelude

------------------------------------------
-- Builders

data Credentials =
    AWSProfile Text
  | EnvironmentVariables

data AWSVariables t =
  AWSVariables {
      awsRegion :: Text
    , awsCredentials :: Credentials
    , awsType :: t
    }

data AWSAmiOwner =
    Accounts [Text]
  | Self
  | Alias Text

data SourceAmi =
    SourceAmiId Text
  | SourceAmiFilter (Map Text Text) AWSAmiOwner Bool

data SourceAmiFilterKey =
    -- | The image architecture (i386 | x86_64).
    Architecture
    -- | A Boolean value that indicates whether the Amazon EBS volume is deleted on instance termination.
  | BlockDeviceMappingDeleteOnTermination
    -- | The device name specified in the block device mapping (for example, /dev/sdh or xvdh).rmination
  | BlockDeviceMappingDeviceName
  -- | The ID of the snapshot used for the EBS volume.
  | BlockDeviceMappingSnapshotId
  -- | The volume size of the EBS volume, in GiB.
  | BlockDeviceMappingVolumeSize
  -- | The volume type of the EBS volume (gp2 | io1 | st1 | sc1 | standard).
  | BlockDeviceMappingVolumeType
    -- | A Boolean that indicates whether the EBS volume is encrypted.e
  | BlockDevice
    -- | The description of the image (provided during image creation).-mapping.encrypted
  | Description
  -- | A Boolean that indicates whether enhanced networking with ENA is enabled.
  | EnaSupport
  -- | The hypervisor type (ovm | xen).
  | Hypervisor
    -- | The ID of the image.r
  | ImageId
  -- | The image type (machine | kernel | ramdisk).
  | ImageType
  -- | A Boolean that indicates whether the image is public.
  | IsPublic
  -- | The kernel ID.
  | KernelId
  -- | The location of the image manifest.
  | ManifestLocation
    -- | The name of the AMI (provided during image creation).est-location
  | Name
  -- | String value from an Amazon-maintained list (amazon | aws-marketplace | microsoft) of snapshot owners. Not to be confused with the user-configured AWS account alias, which is set from the IAM console.
  | OwnerAlias
    -- | The AWS account ID of the image owner.as
  | OwnerId
  -- | The platform. To only list Windows-based AMIs, use windows.
  | Platform
  -- | The product code.
  | ProductCode
  -- | The type of the product code (devpay | marketplace).
  | ProductCodeType
    -- | The RAM disk ID
  | RamdiskId
  -- | The device name of the root device volume (for example, /dev/sda1).
  | RootDeviceName
  -- | The type of the root device volume (ebs | instance-store).
  | RootDeviceType
    -- | The state of the image (available | pending | failed).evice-type
  | State
  -- | The reason code for the state change.
  | StateReasonCode
  -- | The message for the state change.
  | StateReasonMessage
    -- | A value of simple indicates that enhanced networking with the Intel 82599 VF interface is enabled.ge
  | SriovNetSupport
    -- | The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key Owner and the value TeamA, specify tag:Owner for the filter name and TeamA for the filter value.support
  | Tag Text
    -- | The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.>
  | TagKey
  -- | The virtualization type (paravirtual | hvm).
  | VirtualizationType

{--
renderSourceAmiFilterKey :: SourceAmiFilterKey -> Text
renderSourceAmiFilterKey = \case
    architecture
  | block-device-mapping.delete-on-termination
  | block-device-mapping.device-name
  | block-device-mapping.snapshot-id
  | block-device-mapping.volume-size
  | block-device-mapping.volume-type
  | block-device-mapping.encrypted
  | description
  | ena-support
  | hypervisor
  | image-id
  | image-type
  | is-public
  | kernel-id
  | manifest-location
  | name
  | owner-alias
  | owner-id
  | platform
  | product-code
  | product-code.type
  | ramdisk-id
  | root-device-name
  | root-device-type
  | state
  | state-reason-code
  | state-reason-message
  | sriov-net-support
  | tag:<key>
  | tag-key
  | virtualization-type

--}




-- https://github.com/hashicorp/packer/blob/5504709e1dba015b5e7858e9a870ad4ff7bf7b6e/builder/amazon/common/block_device.go
-- NoDevice | Ephemeral (+ Name) | Mapping *
data BlockDeviceMapping =
  BlockDeviceMapping {
      blockDeviceMappingName :: Text
    , blockDeviceMappingVolumeType :: Text
    , blockDeviceMappingIOOPS :: Maybe Int -- only when VolumeType "io1" (/ "gp2" / "standard")
    , blockDeviceMappingVolumeSize :: Int
    , blockDeviceMappingDeleteOnTermination :: Bool
    , blockDeviceMappingEncrypted :: Bool -- incompatible with 'SnapshotId'
    , blockDeviceMappingKMS :: Maybe Text -- only when Encrypted
    , blockDeviceMappingSnapshotId :: Maybe Text
    , blockDeviceMappingVirtualName :: Maybe Text -- prefixed with 'ephemeral' for
    , blockDeviceMappingNoDevice :: Maybe Bool
    }

blockDeviceMapping :: Text -> Text -> Int -> Bool -> BlockDeviceMapping
blockDeviceMapping name vtype vsize delete =
  BlockDeviceMapping {
      blockDeviceMappingName = name
    , blockDeviceMappingVolumeType = vtype
    , blockDeviceMappingIOOPS = Nothing
    , blockDeviceMappingVolumeSize = vsize
    , blockDeviceMappingDeleteOnTermination = delete
    , blockDeviceMappingEncrypted = False
    , blockDeviceMappingKMS = Nothing
    , blockDeviceMappingSnapshotId = Nothing
    , blockDeviceMappingVirtualName = Nothing
    , blockDeviceMappingNoDevice = Nothing
    }

-- | Amazon AMI Builder
--
-- Create EBS-backed AMIs by launching a source AMI and re-packaging
-- it into a new AMI after provisioning. If in doubt, use this
-- builder, which is the easiest to get started with.
--
-- https://www.packer.io/docs/builders/amazon-ebs.html
--
-- 'amazon-ebs'
--
data EBS =
  EBS {
      ebsAccountId :: Text -- ^ account_id
    , ebsAmiName :: Text -- ^ ami_name
    -- ami_description
    -- ami_users
    -- ami_regions

    , ebsInstanceType :: Text -- ^ instance_type
    , ebsRegion :: Text -- ^ region
    , ebsSourceAmi :: SourceAmi -- ^ source_ami
    , ebsS3Bucket :: Maybe Text -- ^ s3_bucket
    -- TODO x509_cert_path
    -- TODO x509_key_path

    , ebsLaunchBlockDeviceMappings :: [BlockDeviceMapping] -- ^ launch_block_device_mappings

    -- encrypt_boot
    -- kms_key_id

    , ebsVpcId :: Text -- ^ vpc_id
    -- TODO vpc_filter - https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeVpcs.html
    , ebsSubnetId :: Text -- ^ subnet_id
    -- TODO subnet_filter - https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeSubnets.html
    -- temporary_security_group_source_cidrs

    , ebsAssociatePublicIpAddress :: Bool -- ^ associate_public_ip_address
    , ebsIAMInstanceProfile :: Maybe Text -- ^ iam_instance_profile
    , ebsInsecureSkipTLSVerify :: Maybe Bool -- ^ insecure_skip_tls_verify

    , ebsRunTags :: Map Text Text -- ^ run_tags
    , ebsTags :: Map Text Text -- ^ tags


    }

-- amazon-instance

data Instance =
  Instance {
      instanceAccountId :: Text
-- ami_name
-- instance_type
-- region
-- s3_bucket
-- source_ami
    }

-- amazon-chroot

-- ami_name
-- source_ami

-- amazon-ebssurrogate

-- instance_type
-- region
-- source_ami
-- ami_root_device


------------------------------------------
