{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import           Control.Applicative ((<**>))

import           Options.Applicative (Parser, CommandFields, Mod)
import qualified Options.Applicative as Options

import qualified System.IO as IO
import qualified System.Exit as Exit

import           Kerry.Command.Reverse
import           Kerry.Internal.Prelude


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  run =<<
    Options.customExecParser
      (Options.prefs Options.showHelpOnEmpty)
      (Options.info (parser <**> Options.helper) mempty)

data Command =
    KerryReverse !Reverse
    deriving (Eq, Ord, Show)

parser :: Parser Command
parser =
  Options.subparser $ mconcat commands

command :: Parser a -> String -> String -> Mod CommandFields a
command p name desc =
  Options.command name (Options.info (p <**> Options.helper) (Options.progDesc desc))

commands :: [Mod CommandFields Command]
commands = [
    command
      (KerryReverse <$> pReverse)
      "reverse"
      "Reverses a list of arguments."
  ]

pSeparator :: Parser String
pSeparator =
  Options.option Options.str $ mconcat [
      Options.long "separator"
    , Options.short 's'
    , Options.metavar "SEPARATOR"
    , Options.help "The separator to use in the output."
    ]

pInput :: Parser String
pInput =
  Options.argument Options.str $ mconcat [
      Options.metavar "INPUT"
    , Options.help "The input to reverse."
    ]

pReverse :: Parser Reverse
pReverse =
  Reverse
    <$> many pInput
    <*> pSeparator

orDie :: (x -> String) -> ExceptT x IO a -> IO a
orDie render io =
  let
    onError err = do
      IO.hPutStrLn IO.stderr $ render err
      Exit.exitWith (Exit.ExitFailure 1)
  in
    either onError pure =<< runExceptT io

run :: Command -> IO ()
run = \case
  KerryReverse x ->
    orDie show $ kerryReverse x
