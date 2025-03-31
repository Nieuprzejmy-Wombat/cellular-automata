{-# LANGUAGE DeriveFunctor #-}

module Grid where

import Data.Array

class (Functor w) => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

data Grid s a = Grid {grid :: Array s a, focus :: s}
  deriving (Functor, Show)

genGrid :: (Ix s) => (s, s) -> (s -> a) -> Grid s a
genGrid (beginning, end) gen = Grid {focus = beginning, grid = array (beginning, end) [(coord, gen coord) | coord <- range (beginning, end)]}

defaultGrid :: (Ix s, Default a) => (s, s) -> Grid s a
defaultGrid = flip genGrid (const def)

instance (Coord s) => Comonad (Grid s) where
  extract (Grid g s) = g ! wrap (bounds g) s
  duplicate g = g {grid = array (bounds $ grid g) . fmap (\s -> (s, g {focus = s})) . indices . grid $ g}

class Default a where
  def :: a

instance Default Bool where
  def = False

class (Ix a) => Coord a where
  wrap :: (a, a) -> a -> a
  neighbours :: a -> [a]
  centered :: a -> (a, a)

instance Coord Int where
  wrap (low, high) n
    | n > high = low - 1 + (n - high)
    | n < low = high + 1 - (low - n)
    | otherwise = n
  neighbours a = [pred a, succ a]
  centered a = (negate (a `div` 2), a `div` 2)

instance (Coord a, Coord b) => Coord (a, b) where
  wrap ((lowx, lowy), (highx, highy)) (nx, ny) = (wrap (lowx, highx) nx, wrap (lowy, highy) ny)
  neighbours (x, y) = [(i, j) | i <- x : neighbours x, j <- y : neighbours y, (i, j) /= (x, y)]
  centered (x, y) = case (centered x, centered y) of
    ((lowx, highx), (lowy, highy)) -> ((lowx, lowy), (highx, highy))

instance (Coord a, Coord b, Coord c) => Coord (a, b, c) where
  wrap ((lowx, lowy, lowz), (highx, highy, highz)) (nx, ny, nz) = (wrap (lowx, highx) nx, wrap (lowy, highy) ny, wrap (lowz, highz) nz)
  neighbours (x, y, z) = [(i, j, k) | i <- x : neighbours x, j <- y : neighbours y, k <- z : neighbours z, (i, j, k) /= (x, y, z)]
  centered (x, y, z) = case (centered x, centered y, centered z) of
    ((lowx, highx), (lowy, highy), (lowz, highz)) -> ((lowx, lowy, lowz), (highx, highy, highz))
