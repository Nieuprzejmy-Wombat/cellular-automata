module Grid where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Array
import Data.Bool (bool)
import System.Console.ANSI
import System.IO

type Grid = Array Int

neighbours :: Grid Bool -> Int -> (Bool, Bool)
neighbours grid x = (x /= fst (bounds grid) && grid ! pred x, x /= snd (bounds grid) && grid ! succ x)

type Rule a = (a, a) -> a -> a

step :: Grid Bool -> Rule Bool -> Grid Bool
step grid rule = array (bounds grid) $ (\(x, a) -> (x, rule (neighbours grid x) a)) <$> assocs grid

pprint :: Grid Bool -> IO ()
pprint = putStr . fmap (bool '⬛' '⬜' . snd) . assocs

data Config = Config {start :: Grid Bool, rule :: Rule Bool}

exConfig :: Config
exConfig = Config (array (begin, end) [(i, False) | i <- [begin .. end]] // [(0, True), (2, True), (4, True)]) exRule
 where
  exRule (p, r) q = p /= (q || r)
  begin = -35
  end = 35

loop :: Config -> IO ()
loop cfg = void $ saveCursor *> clearScreen *> loop' (start cfg)
 where
  loop' arr = pprint arr *> hFlush stdout *> restoreCursor *> clearScreen *> threadDelay 1000000 *> loop' (step arr (rule cfg))
