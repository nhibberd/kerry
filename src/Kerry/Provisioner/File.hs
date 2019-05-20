{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Kerry.Provisioner.File (
  -- * File provisioner
    File(..)
  , FileDirection(..)

  -- ** Serialziation
  , fromFile
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson.Types as Aeson

import           Kerry.Internal.Prelude
import           Kerry.Internal.Serial


data File =
  File {
    -- | The path to a local file or directory to upload to the machine. The path can be absolute or relative. If it is relative, it is relative to the working directory when Packer is executed. If this is a directory, the existence of a trailing slash is important. Read below on uploading directories.
      fileSource :: Text
    -- | The path where the file will be uploaded to in the machine. This value must be a writable location and any parent directories must already exist. If the provisioning user (generally not root) cannot write to this directory, you will receive a "Permission Denied" error. If the source is a file, it's a good idea to make the destination a file as well, but if you set your destination as a directory, at least make sure that the destination ends in a trailing slash so that Packer knows to use the source's basename in the final upload path. Failure to do so may cause Packer to fail on file uploads. If the destination file already exists, it will be overwritten.
    , fileDestination :: Text
    -- | The direction of the file transfer. This defaults to "upload". If it is set to "download" then the file "source" in the machine will be downloaded locally to "destination"
    , fileDirection :: FileDirection
    -- | For advanced users only. If true, check the file existence only before uploading, rather than upon pre-build validation. This allows to upload files created on-the-fly. This defaults to false. We don't recommend using this feature, since it can cause Packer to become dependent on system state. We would prefer you generate your files before the Packer run, but realize that there are situations where this may be unavoidable.
    , fileGenerated :: Maybe Bool
    } deriving (Eq, Show)

-- | 'FileDirection'
data FileDirection =
    FileUpload
  | FileDownload
    deriving (Eq, Show)

renderFileDirection :: FileDirection -> Text
renderFileDirection = \case
  FileUpload ->
    "upload"
  FileDownload ->
    "download"

fromFile :: File -> [Aeson.Pair]
fromFile f =
  join [[
    "type" .= t "file"
  , "source" .= (fileSource f)
  , "destination" .= (fileDestination f)
  , "direction" .= (renderFileDirection $ fileDirection f)
  ]
  , "generated" .=? (fileGenerated f)
  ]
