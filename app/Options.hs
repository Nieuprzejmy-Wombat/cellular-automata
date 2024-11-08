module Options (Config (..), opts) where

import Options.Applicative

data Config = Config {width :: Int, height :: Int, time :: Int}

config :: Parser Config
config = Config <$> w <*> h <*> t
 where
  w = option auto $ long "width" <> short 'w' <> metavar "INT" <> value 10 <> help "width of the grid"
  h = option auto $ long "height" <> short 'h' <> metavar "INT" <> value 10 <> help "height of the grid"
  t = option auto $ long "time" <> short 't' <> metavar "INT" <> value 1000 <> help "time to wait between ticks in milliseconds"

opts :: ParserInfo Config
opts = info (config <**> helper) $ fullDesc <> progDesc "simulate a cellular automaton" <> header "cellular automata simulator"
