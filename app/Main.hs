module Main where

import Grid (loop)
import Options (opts)
import Options.Applicative (execParser)

main :: IO ()
main = execParser opts >>= loop
