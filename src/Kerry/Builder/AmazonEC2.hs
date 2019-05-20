{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kerry.Builder.AmazonEC2 (
  -- * General AmazonEC2 Builder
    Credentials (..)
  , AWS (..)
  , fromAWS

  -- * Utilities
  , SourceAmi (..)
  , AWSAmiOwner (..)
  , SourceAmiFilterKey (..)
  , BlockDeviceMapping (..)
  , blockDeviceMapping

  -- * Builders
  -- ** EBS Backed
  , EBS (..)
  , fromEBS

  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as Map

import           Kerry.Prelude
import           Kerry.Serial

data Credentials =
    AWSProfile Text
  | EnvironmentVariables
  -- TODO explicit keys?
    deriving (Eq, Show)

data AWS x =
  AWS {
      awsRegion :: Text
    , awsCredentials :: Credentials
    , awsBuilder :: x
    } deriving (Eq, Show)

fromAWS :: (a -> [Aeson.Pair]) -> AWS a -> [Aeson.Pair]
fromAWS fromBuilder (AWS region creds builder) =
  join [
      ["region" .= region]
    , case creds of
        AWSProfile profile ->
          ["profile" .= profile]
        EnvironmentVariables ->
          []
    , fromBuilder builder
    ]

data SourceAmi =
    SourceAmiId Text
  | SourceAmiFilter (Map SourceAmiFilterKey Text) AWSAmiOwner Bool
    deriving (Eq, Show)

data AWSAmiOwner =
    Accounts [Text]
  | Self
  | Alias Text
    deriving (Eq, Show)

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
    deriving (Eq, Show)

renderSourceAmiFilterKey :: SourceAmiFilterKey -> Text
renderSourceAmiFilterKey = \case
  Architecture ->
    "architecture"
  BlockDeviceMappingDeleteOnTermination ->
    "block-device-mapping.delete-on-termination"
  BlockDeviceMappingDeviceName ->
    "block-device-mapping.device-name"
  BlockDeviceMappingSnapshotId ->
    "block-device-mapping.snapshot-id"
  BlockDeviceMappingVolumeSize ->
    "block-device-mapping.volume-size"
  BlockDeviceMappingVolumeType ->
    "block-device-mapping.volume-type"
  BlockDevice ->
    "block-device-mapping.encrypted"
  Description ->
    "description"
  EnaSupport ->
    "ena-support"
  Hypervisor ->
    "hypervisor"
  ImageId ->
    "image-id"
  ImageType ->
    "image-type"
  IsPublic ->
    "is-public"
  KernelId ->
    "kernel-id"
  ManifestLocation ->
    "manifest-location"
  Name ->
    "name"
  OwnerAlias ->
    "owner-alias"
  OwnerId ->
    "owner-id"
  Platform ->
    "platform"
  ProductCode ->
    "product-code"
  ProductCodeType ->
    "product-code.type"
  RamdiskId ->
    "ramdisk-id"
  RootDeviceName ->
    "root-device-name"
  RootDeviceType ->
    "root-device-type"
  State ->
    "state"
  StateReasonCode ->
    "state-reason-code"
  StateReasonMessage ->
    "state-reason-message"
  SriovNetSupport ->
    "sriov-net-support"
  Tag key ->
    "tag:" <> key
  TagKey ->
    "tag-key"
  VirtualizationType ->
    "virtualization-type"

-- https://github.com/hashicorp/packer/blob/5504709e1dba015b5e7858e9a870ad4ff7bf7b6e/builder/amazon/common/block_device.go
-- NoDevice | Ephemeral (+ Name) | Mapping *
data BlockDeviceMapping =
  BlockDeviceMapping {
    -- | The device name exposed to the instance (for example, /dev/sdh or xvdh). Required for every device in the block device mapping.
      blockDeviceMappingName :: Text
    -- | The volume type. gp2 for General Purpose (SSD) volumes, io1 for Provisioned IOPS (SSD) volumes, and standard for Magnetic volumes
    , blockDeviceMappingVolumeType :: Text
    -- | The number of I/O operations per second (IOPS) that the volume supports. See the documentation on IOPs for more information
    , blockDeviceMappingIOPS :: Maybe Int -- only when VolumeType "io1" (/ "gp2" / "standard")
    -- | The size of the volume, in GiB. Required if not specifying a snapshot_id
    , blockDeviceMappingVolumeSize :: Int
    -- | Indicates whether the EBS volume is deleted on instance termination. Default false. NOTE: If this value is not explicitly set to true and volumes are not cleaned up by an alternative method, additional volumes will accumulate after every build.
    , blockDeviceMappingDeleteOnTermination :: Bool
    -- | Indicates whether or not to encrypt the volume. By default, Packer will keep the encryption setting to what it was in the source image. Setting false will result in an unencrypted device, and true will result in an encrypted one.
    , blockDeviceMappingEncrypted :: Bool -- incompatible with 'SnapshotId'
    -- | KMS Key ID to use when 'encrytped' is set to True.
    , blockDeviceMappingKMS :: Maybe Text -- only when Encrypted
    -- | The ID of the snapshot
    , blockDeviceMappingSnapshotId :: Maybe Text
    -- | Suppresses the specified device included in the block device mapping of the AMI
    , blockDeviceMappingVirtualName :: Maybe Text -- prefixed with 'ephemeral' for
    -- | The virtual device name. See the documentation on Block Device Mapping for more information
    , blockDeviceMappingNoDevice :: Maybe Bool
    } deriving (Eq, Show)

blockDeviceMapping :: Text -> Text -> Int -> Bool -> BlockDeviceMapping
blockDeviceMapping name vtype vsize delete =
  BlockDeviceMapping {
      blockDeviceMappingName = name
    , blockDeviceMappingVolumeType = vtype
    , blockDeviceMappingIOPS = Nothing
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
data EBS =
  EBS {
    -- Required
      ebsAmiName :: Text -- ^ ami_name
    , ebsSourceAmi :: SourceAmi -- ^ source_ami
    , ebsInstanceType :: Text -- ^ instance_type

    -- Optional
    , ebsAmiDescription :: Maybe Text -- ^ ami_description
    -- ami_groups
    -- ami_product_codes
    , ebsAmiRegions :: Maybe [Text] -- ^ ami_regions
    , ebsAmiUsers :: Maybe [Text] -- ^ ami_users
    -- ami_virtualization_type
    , ebsAssociatePublicIpAddress :: Maybe Bool -- ^ associate_public_ip_address
    , ebsAvailabilityZone :: Maybe Text -- ^ availability_zone
    -- block_duration_minutes
    -- custom_endpoint_ec2
    -- decode_authorization_messages
    -- disable_stop_instance
    -- ebs_optimized
    -- ena_support
    -- enable_t2_unlimited
    -- encrypt_boot
    -- kms_key_id
    -- force_delete_snapshot
    -- force_deregister
    , ebsIAMInstanceProfile :: Maybe Text -- ^ iam_instance_profile
    , ebsInsecureSkipTLSVerify :: Maybe Bool -- ^ insecure_skip_tls_verify
    , ebsLaunchBlockDeviceMappings :: [BlockDeviceMapping] -- ^ launch_block_device_mappings
    -- mfa_code
    -- profile
    -- region_kms_key_ids
    , ebsRunTags :: Map Text Text -- ^ run_tags
    -- run_volume_tags
    -- security_group_id
    -- security_group_ids
    -- security_group_filter
    -- shutdown_behavior
    -- skip_region_validation
    -- snapshot_groups
    -- snapshot_users
    -- snapshot_tags
    -- spot_price
    -- spot_price_auto_product
    -- spot_tags
    -- sriov_support
    -- ssh_keypair_name
    -- ssh_agent_auth
    -- ssh_interface
    , ebsSubnetId :: Maybe Text -- ^ subnet_id
    -- subnet_filter - https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeSubnets.html
    , ebsTags :: Map Text Text -- ^ tags
    -- temporary_key_pair_name
    -- temporary_security_group_source_cidrs
    -- token
    -- user_data
    -- user_data_file
    -- vault_aws_engine
    , ebsVpcId :: Maybe Text -- ^ vpc_id
    -- vpc_filter - https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeVpcs.html
    -- windows_password_timeout
    } deriving (Eq, Show)


fromEBS :: EBS -> [Aeson.Pair]
fromEBS ebs = join [
    ["ami_name" .= ebsAmiName ebs]
  , [fromSourceAmi $ ebsSourceAmi ebs]
  , ["instance_type" .= ebsInstanceType ebs]
  , "ami_description" .=? ebsAmiDescription ebs
  , "ami_regions" .=? ebsAmiRegions ebs
  , "ami_users" .=? ebsAmiUsers ebs
  , "associate_public_ip_address" .=? ebsAssociatePublicIpAddress ebs
  , "availability_zone" .=? ebsAvailabilityZone ebs
  , "iam_instance_profile" .=? ebsIAMInstanceProfile ebs
  , "insecure_skip_tls_verify" .=? ebsInsecureSkipTLSVerify ebs
  , "launch_block_device_mappings" .=? list (fromBlockDeviceMapping <$> ebsLaunchBlockDeviceMappings ebs)
  , ["run_tags" .= fromMap (ebsRunTags ebs)]
  , "subnet_id" .=? ebsSubnetId ebs
  , ["tags" .= fromMap (ebsTags ebs)]
  , "vpc_id" .=? ebsVpcId ebs
  ]

fromSourceAmi :: SourceAmi -> Aeson.Pair
fromSourceAmi = \case
  SourceAmiId x ->
    "source_ami" .= x
  SourceAmiFilter filters owner recent ->
    "source_ami_filter" .= Aeson.object [
        "filters" .= fromMap (Map.mapKeys renderSourceAmiFilterKey filters)
      , "owners" .= case owner of
          Accounts as ->
            Aeson.toJSON as
          Self ->
            Aeson.String "self"
          Alias a ->
            Aeson.String a
      , "most_recent" .= recent
      ]

fromBlockDeviceMapping :: BlockDeviceMapping -> Aeson.Value
fromBlockDeviceMapping b =
  Aeson.object $ join [[
      "device_name" .= blockDeviceMappingName b
    , "volume_type" .= blockDeviceMappingVolumeType b
    , "volume_size" .= blockDeviceMappingVolumeSize b
    , "delete_on_termination" .= blockDeviceMappingDeleteOnTermination b
    , "encrypted" .= blockDeviceMappingEncrypted b
    ]
    , "iops" .=? blockDeviceMappingIOPS b
    , "kms_key_id" .=? blockDeviceMappingKMS b
    , "snapshot_id" .=? blockDeviceMappingSnapshotId b
    , "virtual_name" .=? blockDeviceMappingVirtualName b
    , "no_device" .=? blockDeviceMappingNoDevice b
    ]

fromMap :: Map Text Text -> Aeson.Value
fromMap m =
  Aeson.object . flip fmap (Map.toList m) $ \(k, v) ->
    k .= v
