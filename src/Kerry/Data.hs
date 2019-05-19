{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Kerry.Data (
  -- * Data
    Variable(..)
  , Builder(..)
  , BuilderType(..)
  , Provisioner(..)
  , PostProcessor(..)
  , Packer(..)

  , Communicator(..)
  , SSHCommunicator(..)
  , defaultSSHCommunicator

  -- * Serialization
  , fromPacker
  , fromBuilder
  , fromBuilderType
  , fromVariable
  , fromProvisioner
  , fromPostProcessor
  ) where

import           Data.Aeson (Value, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Map.Strict as Map

import           Kerry.Prelude
import qualified Kerry.Builder.AmazonEC2 as AmazonEC2
import           Kerry.Serial

data Variable =
    Variable Text Text
    deriving (Eq, Show)

-- This type variable is wrong
data Builder =
  Builder {
      builderType :: BuilderType
    , builderName :: Maybe Text
    , builderCommunicator :: Communicator
    } deriving (Eq, Show)

data BuilderType =
    AmazonEBSBuilder AmazonEC2.EBS
    deriving (Eq, Show)

data Provisioner =
  Provisioner {
      provisionerType :: Text
      -- options not expressive enough
    , provisionerOptions :: Map Text Text
    , provisionerOnly :: [Text]
    , provisionerExcept :: [Text]
    , provisionerPauseBefore :: Maybe Text
    , provisionerTimeout :: Maybe Text
    , provisionerOverride :: Map Text (Map Text Text)
    } deriving (Eq, Show)

data PostProcessor =
  PostProcessor
  deriving (Eq, Show)

-- This type variable is wrong
data Packer =
  Packer {
      variables :: [Variable]
    , builders :: [Builder]
    , provisioners :: [Provisioner]
    , postProcessors :: [PostProcessor]
    } deriving (Eq, Show)


-- https://www.packer.io/docs/templates/communicator.html
data Communicator =
    None
  | SSH SSHCommunicator
  | WinRm
    deriving (Eq, Show)


-- https://www.packer.io/docs/templates/communicator.html#ssh
data SSHCommunicator =
  SSHCommunicator {
      sshUsername :: Text -- ^ 'ssh_username' - The username to connect to SSH with. Required if using SSH.
    , sshPty :: Bool -- ^ 'ssh_pty' - If true, a PTY will be requested for the SSH connection. This defaults to false.
    , sshTimeout :: Int -- ^ 'ssh_timeout' (string) - The time to wait for SSH to become available. Packer uses this to determine when the machine has booted so this is usually quite long. Example value: 10.
-- 'ssh_agent_auth' (boolean) - If true, the local SSH agent will be used to authenticate connections to the remote host. Defaults to false.
-- 'ssh_bastion_agent_auth' (boolean) - If true, the local SSH agent will be used to authenticate with the bastion host. Defaults to false.
-- 'ssh_bastion_host' (string) - A bastion host to use for the actual SSH connection.
-- 'ssh_bastion_password' (string) - The password to use to authenticate with the bastion host.
-- 'ssh_bastion_port' (number) - The port of the bastion host. Defaults to 22.
-- 'ssh_bastion_private_key_file' (string) - Path to a PEM encoded private key file to use to authenticate with the bastion host. The ~ can be used in path and will be expanded to the home directory of current user.
-- 'ssh_bastion_username' (string) - The username to connect to the bastion host.
-- 'ssh_clear_authorized_keys' (boolean) - If true, Packer will attempt to remove its temporary key from ~/.ssh/authorized_keys and /root/.ssh/authorized_keys. This is a mostly cosmetic option, since Packer will delete the temporary private key from the host system regardless of whether this is set to true (unless the user has set the -debug flag). Defaults to "false"; currently only works on guests with sed installed.
-- 'ssh_disable_agent_forwarding' (boolean) - If true, SSH agent forwarding will be disabled. Defaults to false.
-- 'ssh_file_transfer_method' (scp or sftp) - How to transfer files, Secure copy (default) or SSH File Transfer Protocol.
-- 'ssh_handshake_attempts' (number) - The number of handshakes to attempt with SSH once it can connect. This defaults to 10.
-- 'ssh_host' (string) - The address to SSH to. This usually is automatically configured by the builder.
-- 'ssh_keep_alive_interval' (string) - How often to send "keep alive" messages to the server. Set to a negative value (-1s) to disable. Example value: 10s. Defaults to 5s.
-- 'ssh_password' (string) - A plaintext password to use to authenticate with SSH.
-- 'ssh_port' (number) - The port to connect to SSH. This defaults to 22.
-- 'ssh_private_key_file' (string) - Path to a PEM encoded private key file to use to authenticate with SSH. The ~ can be used in path and will be expanded to the home directory of current user.
-- 'ssh_proxy_host' (string) - A SOCKS proxy host to use for SSH connection
-- 'ssh_proxy_password' (string) - The password to use to authenticate with the proxy server. Optional.
-- 'ssh_proxy_port' (number) - A port of the SOCKS proxy. Defaults to 1080.
-- 'ssh_proxy_username' (string) - The username to authenticate with the proxy server. Optional.
-- 'ssh_read_write_timeout' (string) - The amount of time to wait for a remote command to end. This might be useful if, for example, packer hangs on a connection after a reboot. Example: 5m. Disabled by default.
    } deriving (Eq, Show)

defaultSSHCommunicator :: Text -> SSHCommunicator
defaultSSHCommunicator username =
  SSHCommunicator {
      sshUsername = username
    , sshPty = True
    , sshTimeout = 10
    }



----


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
fromBuilder (Builder btype name comm) =
  A.object $ join [
      fromBuilderType btype
    , "name" .=? name
    , fromCommunicator comm
    ]

fromBuilderType :: BuilderType -> [A.Pair]
fromBuilderType = \case
  AmazonEBSBuilder ebs ->
    "type" .= t "amazon-ebs" : AmazonEC2.fromEBS ebs

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
