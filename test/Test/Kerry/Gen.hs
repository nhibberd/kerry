{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Kerry.Gen (
    genPacker
  , genBuilder
  , genCommunicator
  , genPostProcessor
  ) where

import qualified Data.List as List
import qualified Data.Text as T

import           Hedgehog
import qualified Hedgehog.Corpus as Corpus
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Kerry.Builder.AmazonEC2
import           Kerry.Packer
import           Kerry.Provisioner.File
import           Kerry.Provisioner.Shell
import           Kerry.Internal.Prelude


genPacker :: Gen Packer
genPacker =
  Packer
    <$> Gen.list (Range.linear 0 10) genUserVariable
    <*> genBuilders
    <*> Gen.list (Range.linear 0 10) genProvisioner
    <*> Gen.list (Range.linear 0 0) genPostProcessor

-- TODO functions
genUserVariable :: Gen UserVariable
genUserVariable =
  UserVariable
    <$> Gen.text (Range.linear 3 10) Gen.alphaNum
    <*> Gen.text (Range.linear 3 10) Gen.alphaNum

genBuilders ::  Gen [Builder]
genBuilders = do
  bs <- Gen.list (Range.linear 1 10) genBuilder
  pure $ List.zipWith ($) bs [1..]

genBuilder :: Gen (Int -> Builder)
genBuilder = do
  comm <- SSH <$> genSSHCommunicator
  name <- Gen.maybe $ Gen.element Corpus.agile
  ebsb <- genEBSBuilder
  aws <- genAWS ebsb
  pure $ \index ->
    Builder
      (AmazonEBSBuilder aws)
      ((\n -> n <> "-" <> T.pack (show index)) <$> name)
      comm


genAWS :: MonadGen m => builder -> m (AWS builder)
genAWS builder =
  AWS
    <$> pure "us-west-2"
    <*> Gen.element [EnvironmentVariables, AWSProfile "test-profile"]
    <*> pure builder

genEBSBuilder :: MonadGen m => m EBS
genEBSBuilder =
  EBS
    <$> Gen.text (Range.linear 3 10) Gen.alphaNum
    <*> (SourceAmiId . (<>) "ami-" <$> Gen.text (Range.singleton 8) Gen.alphaNum)
    <*> Gen.element ["t2.micro", "m4.xlarge", "x1.large"]
    <*> Gen.maybe (Gen.text (Range.singleton 8) Gen.alphaNum)
    <*> Gen.maybe (pure ["us-east-1"])
    <*> Gen.maybe (pure ["123456789123", "123456789456"])
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe (pure "us-west-2a")
    <*> Gen.maybe (pure "packer")
    <*> Gen.maybe Gen.bool
    <*> pure []
    <*> genMap
    <*> Gen.maybe (pure "subnet-1234567abcd12345e")
    <*> genMap
    <*> Gen.maybe (pure "vpc-1234567abcd12345")

genMap :: MonadGen m => m (Map Text Text)
genMap =
  Gen.choice [
      pure mempty
    , Gen.map (Range.linear 0 4) $
        Gen.element (zip Corpus.cooking Corpus.vegetables)
    ]

genCommunicator :: Gen Communicator
genCommunicator =
  Gen.choice [
      pure None
    , SSH <$> genSSHCommunicator
    , pure WinRm
    ]

genSSHCommunicator:: MonadGen m => m SSHCommunicator
genSSHCommunicator =
  SSHCommunicator
    <$> Gen.text (Range.linear 3 10) Gen.alpha
    <*> Gen.bool
    <*> Gen.int (Range.linear 0 10)

genProvisioner :: Gen Provisioner
genProvisioner =
  Provisioner
    <$> genProvisionerType
    <*> pure []
    <*> pure []
    <*> pure Nothing
    <*> pure Nothing
    <*> pure mempty

genProvisionerType :: Gen ProvisionerType
genProvisionerType =
  Gen.choice [
      ShellProvisioner <$> genShell
    , FileProvisioner <$> genFile
    ]

genShell :: Gen Shell
genShell = do
  stype <- Gen.element [
      Inline ["echo foo"]
-- TODO files must exist during validation
--    , Script "run.sh"
--    , Scripts ["run.sh", "run2.sh"]
    ]
  pure $ shell stype

genFile :: Gen File
genFile =
  File
    <$> pure "/dev/null"
    <*> pure "/tmp"
    <*> Gen.element [FileUpload, FileDownload]
    <*> Gen.maybe Gen.bool

genPostProcessor :: Gen PostProcessor
genPostProcessor =
  pure PostProcessor
