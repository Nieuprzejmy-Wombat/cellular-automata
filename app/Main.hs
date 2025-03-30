module Main where

import Automata
import Options (autoType, opts)
import Options.Applicative (execParser)

main :: IO ()
main =
  execParser opts >>= \cfg -> case autoType cfg of
    "conway" -> loop cfg conway
    "brian" -> loop cfg brian
    other -> putStrLn $ other <> " is not a supported automaton type"
