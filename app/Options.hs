{-# LANGUAGE ScopedTypeVariables #-}

module Options (Config (..), opts, AutomatonType (..)) where

import Options.Applicative

data Config = Config {delta :: Int, autoType :: AutomatonType}

data AutomatonType = Conway | Brian | OneD String | TwoD String
  deriving (Read)

config :: Parser Config
config = Config <$> d <*> automatonType
  where
    d = option auto $ long "delta" <> short 'd' <> metavar "INT" <> value 1000 <> help "time to wait between ticks in milliseconds"
    automatonType = option auto $ long "type" <> metavar "conway|brian|<rulestring>" <> help "automaton type"

opts :: ParserInfo Config
opts = info (config <**> helper) $ fullDesc <> progDesc "simulate a cellular automaton" <> header "cellular automata simulator"
