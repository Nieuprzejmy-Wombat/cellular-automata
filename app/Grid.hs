{-# LANGUAGE DeriveFunctor #-}

module Grid where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Array
import Data.Bool (bool)
import Options
import System.Console.ANSI
import System.IO

class (Functor w) => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

data Grid s a = Grid {grid :: Array s a, focus :: s}
  deriving (Functor)

instance (Ix s) => Comonad (Grid s) where
  extract g = grid g ! focus g
  duplicate g = g{grid = array (bounds $ grid g) . fmap (\s -> (s, Grid (grid g) s)) . indices . grid $ g}

neighbours :: Grid Int Bool -> (Bool, Bool)
neighbours (Grid g f) = (f /= fst (bounds g) && g ! pred f, f /= snd (bounds g) && g ! succ f)

type Rule a = Grid Int a -> a

pprint :: Grid Int Bool -> IO ()
pprint = putStr . fmap (bool '⬛' '⬜') . elems . grid

thirty :: Rule Bool
thirty g = p /= (extract g || r)
 where
  (p, r) = neighbours g

start :: Config -> Grid Int Bool
start cfg = Grid arr 0
 where
  arr = array (0, width cfg) [(x, False) | x <- [0 .. width cfg]] // [(width cfg `div` 2, True)]

loop :: Config -> IO ()
loop cfg = void $ saveCursor *> clearScreen *> loop' (start cfg)
 where
  loop' g = pprint g *> hFlush stdout *> restoreCursor *> clearScreen *> threadDelay (time cfg * 1000) *> loop' (extend thirty g)
