{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | All strings within templates are processed by a common Packer
-- templating engine, where variables and functions can be used to
-- modify the value of a configuration parameter at runtime.
module Kerry.Functions (
    Template (..)
  , RawTemplate
  , toTemplate
  , renderRawTemplate

  -- * User Variable
  , user

  -- * Template Variable
  , templateVariable

  -- * Functions
  , buildName
  , buildType
  , env
  , isotime
  , lower
  , pwd
  , sed
  , split
  , templateDir
  , timestamp
  , uuid
  , upper
  , packerVersion
  , cleanResourceName
  ) where

import qualified Data.Text as T

import           Kerry.Prelude

newtype Template =
  Template {
      renderTemplate :: Text
    } deriving (Eq, Show)

newtype RawTemplate =
  RawTemplate {
      _rawTemplate :: Text
    } deriving (Eq, Show)

toTemplate :: RawTemplate -> Template
toTemplate (RawTemplate raw) =
  Template $ "{{" <> raw <> "}}"

renderRawTemplate :: RawTemplate -> Text
renderRawTemplate =
  renderTemplate . toTemplate

-- | User variables
user :: Text -> RawTemplate
user variable =
  RawTemplate $ " user `" <> variable <> "` "

-- | Template variables are special variables automatically set by
-- Packer at build time. Some builders, provisioners and other
-- components have template variables that are available only for that
-- component. Template variables are recognizable because they're
-- prefixed by a period, such as {{ .Name }}.
templateVariable :: Text -> Template
templateVariable variable =
  Template $ "{{." <> variable <> "}}"


-- | The name of the build being run.
buildName :: RawTemplate
buildName =
  RawTemplate "build_name"

-- | The type of the builder being used currently.
buildType :: RawTemplate
buildType =
  RawTemplate "build_type"

-- | Returns environment variables. See example in using home variable
env :: Text -> RawTemplate
env variable =
  RawTemplate $ "env `" <> variable <> "`"

-- | UTC time, which can be formatted. See more examples below in the isotime format reference.
isotime :: Text -> RawTemplate
isotime format =
  RawTemplate $ "isotime " <> format

-- | Lowercases the string.
lower :: RawTemplate
lower =
  RawTemplate "lower"

-- | The working directory while executing Packer.
pwd :: RawTemplate
pwd =
  RawTemplate "pwd"

-- | Use a golang implementation of sed to parse an input string.
sed :: Text -> Text -> RawTemplate
sed a b =
  RawTemplate $ "sed " <> a <> " " <> b <> ""

-- | Split an input string using separator and return the requested substring.
split :: Text -> Text -> Int -> RawTemplate
split a b c =
  RawTemplate $ "split " <> a <> " " <> b <> " " <> (T.pack $ show c) <> ""

-- | The directory to the template for the build.
templateDir :: RawTemplate
templateDir =
  RawTemplate "template_dir"

-- | The current Unix timestamp in UTC.
timestamp :: RawTemplate
timestamp =
  RawTemplate "timestamp"

-- | Returns a random UUID.
uuid :: RawTemplate
uuid =
  RawTemplate "uuid"

-- | Uppercases the string.
upper :: RawTemplate
upper =
  RawTemplate "upper"

-- | Returns Packer version.
packerVersion :: RawTemplate
packerVersion =
  RawTemplate "packer_version"

-- | Image names can only contain certain characters and have a
-- maximum length, eg 63 on GCE & 80 on Azure. clean_resource_name
-- will convert upper cases to lower cases and replace illegal
-- characters with a "-" character.
cleanResourceName :: RawTemplate
cleanResourceName =
  RawTemplate "clean_resource_name"
