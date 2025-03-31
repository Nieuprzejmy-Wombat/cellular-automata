{-# LANGUAGE KindSignatures #-}

module Options (Config (..), opts) where

import GHC.Natural
import Options.Applicative

data Config coords = Config {size :: coords, delta :: Int, autoType :: AutomatonType}

type AutomatonType = String

config :: Parser (Config n)
config = Config <$> s <*> d <*> automatonType
  where
    s = argument auto $ metavar "<n-tuple>" <> help "grid size" <> value (Cons 30 (Cons 30 Nil))
    d = option auto $ long "delta" <> short 'd' <> metavar "INT" <> value 1000 <> help "time to wait between ticks in milliseconds"
    automatonType = strOption $ long "type" <> metavar "conway|brian" <> help "automaton type"

opts :: ParserInfo (Config n)
opts = info (config <**> helper) $ fullDesc <> progDesc "simulate a cellular automaton" <> header "cellular automata simulator"
