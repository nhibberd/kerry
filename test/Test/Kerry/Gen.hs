{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Kerry.Gen (
    genPacker
  , genCommunicator
  , genPostProcessor
  ) where

import qualified Data.Map as Map

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Kerry.Builder.AmazonEC2
import           Kerry.Data
import           Kerry.Prelude


genPacker :: Gen Packer
genPacker =
  Packer
    <$> Gen.list (Range.linear 0 10) genVariable
    <*> Gen.list (Range.linear 1 3) genBuilder
    <*> Gen.list (Range.linear 0 10) genProvisioner
    <*> Gen.list (Range.linear 0 0) genPostProcessor

-- TODO functions
genVariable :: Gen Variable
genVariable =
  Variable
    <$> Gen.text (Range.linear 3 10) Gen.alphaNum
    <*> Gen.text (Range.linear 3 10) Gen.alphaNum

genBuilder :: Gen Builder
genBuilder =
  Gen.choice [
      Builder
        <$> (AmazonEBSBuilder <$> genEBSBuilder)
        <*> Gen.maybe (Gen.text (Range.linear 5 5) Gen.alphaNum)
        <*> (SSH <$> genSSHCommunicator)
    ]

genEBSBuilder :: Gen EBS
genEBSBuilder =
  EBS
    <$> Gen.text (Range.linear 3 10) Gen.alphaNum
    <*> (SourceAmiId . (<>) "ami-" <$> Gen.text (Range.singleton 8) Gen.alphaNum)
    <*> Gen.element ["t2.micro", "m4.xlarge", "x1.large"]
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> pure []
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> pure mempty
    <*> pure mempty



genCommunicator :: Gen Communicator
genCommunicator =
  Gen.choice [
      pure None
    , SSH <$> genSSHCommunicator
    , pure WinRm
    ]

genSSHCommunicator:: Gen SSHCommunicator
genSSHCommunicator =
  SSHCommunicator
    <$> Gen.text (Range.linear 3 10) Gen.alpha
    <*> Gen.bool
    <*> Gen.int (Range.linear 0 10)

genProvisioner :: Gen Provisioner
genProvisioner =
  Gen.choice [
      genShellLocalProvisioner
    ]

genShellLocalProvisioner :: Gen Provisioner
genShellLocalProvisioner =
  Provisioner
    <$> pure "shell-local"
    <*> (Gen.element [
        Map.fromList [("command", "echo foo")]
      ])
    <*> pure []
    <*> pure []
    <*> pure Nothing
    <*> pure Nothing
    <*> pure mempty


genPostProcessor :: Gen PostProcessor
genPostProcessor =
  pure PostProcessor
