{-# LANGUAGE TypeApplications #-}

module Main where

import Automata
import Data.Array (Ix (inRange))
import Options (AutomatonType (..), autoType, opts)
import Options.Applicative (execParser)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main =
  execParser opts >>= \cfg -> case dimensions (autoType cfg) of
    1 -> error "1D"
    2 ->
      prompt @(Int, Int) "size: " (minBound, maxBound) >>= \s -> case autoType cfg of
        Conway -> loop s cfg conway
        Brian -> loop s cfg brian
        TwoD _ -> error "2D"
        _ -> error "impossible"
    3 -> error "3D"
    _ -> error "impossible"

prompt :: (Read a, Ix a) => String -> (a, a) -> IO a
prompt p as =
  putStr p *> hFlush stdout *> getLine >>= \l -> case readMaybe l of
    Nothing -> putStrLn "no parse, try again" *> hFlush stdout *> prompt p as
    Just a -> if as `inRange` a then pure a else putStrLn "not in valid range, try again" *> hFlush stdout *> prompt p as

dimensions :: AutomatonType -> Int
dimensions (OneD _) = 1
dimensions (TwoD _) = 2
dimensions _ = 2
