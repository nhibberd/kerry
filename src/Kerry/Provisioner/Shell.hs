{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Kerry.Provisioner.Shell (
  -- * Shell provisioner
    Shell(..)
  , shell

  , ShellType(..)

  -- ** Serialziation
  , fromShell
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson.Types as Aeson

import           Kerry.Internal.Prelude
import           Kerry.Internal.Serial


-- |
-- The shell Packer provisioner provisions machines built by Packer
-- using shell scripts. Shell provisioning is the easiest way to get
-- software installed and configured on a machine.
--
data Shell =
  Shell {
      shellType :: ShellType
    -- | If true, specifies that the script(s) are binary files, and Packer should therefore not convert Windows line endings to Unix line endings (if there are any). By default this is false.
    , shellBinary :: Maybe Bool
    -- | Valid exit codes for the script. By default this is just 0.
    , shellValidExitCodes :: Maybe [Int]
    -- | An array of key/value pairs to inject prior to the execute_command. The format should be key=value. Packer injects some environmental variables by default into the environment, as well, which are covered in the section below.
    , shellEnvironmentVars :: Maybe [Text]
    -- | If true, Packer will write your environment variables to a tempfile and source them from that file, rather than declaring them inline in our execute_command. The default execute_command will be chmod +x {{.Path}}; . {{.EnvVarFile}} && {{.Path}}. This option is unnecessary for most cases, but if you have extra quoting in your custom execute_command, then this may be unnecessary for proper script execution. Default: false.
    , shellUseEnvVarFile :: Maybe Bool
    -- | The command to use to execute the script. By default this is chmod +x {{ .Path }}; {{ .Vars }} {{ .Path }}, unless the user has set "use_env_var_file": true -- in that case, the default execute_command is chmod +x {{.Path}}; . {{.EnvVarFile}} && {{.Path}}. The value of this is treated as a configuration template. There are three available variables:
    --
    -- Path is the path to the script to run
    -- Vars is the list of environment_vars, if configured.
    -- EnvVarFile is the path to the file containing env vars, if use_env_var_file is true.
    , shellExecuteCommand :: Maybe Text
    -- | Defaults to false. Whether to error if the server disconnects us. A disconnect might happen if you restart the ssh server or reboot the host.
    , shellExpectDisconnect :: Maybe Bool
    -- | The shebang value to use when running commands specified by inline. By default, this is /bin/sh -e. If you're not using inline, then this configuration has no effect. Important: If you customize this, be sure to include something like the -e flag, otherwise individual steps failing won't fail the provisioner.
    , shellInlineShebang :: Maybe Text
    -- | The folder where the uploaded script will reside on the machine. This defaults to '/tmp'.
    , shellRemoteFolder :: Maybe FilePath
    -- | The filename the uploaded script will have on the machine. This defaults to 'script_nnn.sh'.
    , shellRemoteFile :: Maybe Text
    -- | The full path to the uploaded script will have on the machine. By default this is remote_folder/remote_file, if set this option will override both remote_folder and remote_file.
    , shellRemotePath :: Maybe FilePath
    -- | If true, specifies that the helper scripts uploaded to the system will not be removed by Packer. This defaults to false (clean scripts from the system).
    , shellSkipClean :: Maybe Bool
    -- | The amount of time to attempt to start the remote process. By default this is 5m or 5 minutes. This setting exists in order to deal with times when SSH may restart, such as a system reboot. Set this to a higher value if reboots take a longer amount of time.
    , shellStartRetryTimeout :: Maybe Text
    -- | Wait the amount of time after provisioning a shell script, this pause be taken if all previous steps were successful.
    , shellPauseAfter :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | Basic 'Shell'
shell :: ShellType -> Shell
shell st =
    Shell {
      shellType = st
    , shellBinary = Nothing
    , shellValidExitCodes = Nothing
    , shellEnvironmentVars = Nothing
    , shellUseEnvVarFile = Nothing
    , shellExecuteCommand = Nothing
    , shellExpectDisconnect = Nothing
    , shellInlineShebang = Nothing
    , shellRemoteFolder = Nothing
    , shellRemoteFile = Nothing
    , shellRemotePath = Nothing
    , shellSkipClean = Nothing
    , shellStartRetryTimeout = Nothing
    , shellPauseAfter = Nothing
    }


-- | 'ShellType'
data ShellType =
    Inline [Text]
  | Script Text
  | Scripts [Text]
    deriving (Eq, Ord, Show)


-- | Shell serialization
fromShell :: Shell -> [Aeson.Pair]
fromShell s =
  join [[
    "type" .= t "shell"
  , fromShellType (shellType s)
  ]
  , "binary" .=? (shellBinary s)
  , "valid_exit_codes" .=? (shellValidExitCodes s)
  , "environment_vars" .=? (shellEnvironmentVars s)
  , "use_env_var_file" .=? (shellUseEnvVarFile s)
  , "execute_command" .=? (shellExecuteCommand s)
  , "expect_disconnect" .=? (shellExpectDisconnect s)
  , "inline_shebang" .=? (shellInlineShebang s)
  , "remote_folder" .=? (shellRemoteFolder s)
  , "remote_file" .=? (shellRemoteFile s)
  , "remote_path" .=? (shellRemotePath s)
  , "skip_clean" .=? (shellSkipClean s)
  , "start_retry_timeout" .=? (shellStartRetryTimeout s)
  , "pause_after" .=? (shellPauseAfter s)
  ]

fromShellType :: ShellType -> Aeson.Pair
fromShellType = \case
  Inline xs ->
    "inline" .= xs
  Script x ->
    "script" .= x
  Scripts xs ->
    "scripts" .= xs
