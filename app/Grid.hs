{-# LANGUAGE DeriveFunctor #-}

module Grid where

import Data.Array
import Data.Array.IArray ((!?))
import Data.Maybe (fromMaybe)

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

instance (Ix s) => Comonad (Grid s) where
  extract (Grid g s) = g ! s -- g ! wrap (bounds g) (x, y)
  duplicate g = g {grid = array (bounds $ grid g) . fmap (\s -> (s, g {focus = s})) . indices . grid $ g}

wrap :: ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int)
wrap ((x0, y0), (x1, y1)) (x, y) = (wrap' (x0, x1) x, wrap' (y0, y1) y)
  where
    wrap' :: (Int, Int) -> Int -> Int
    wrap' (low, high) n
      | n > high = wrap' (low, high) $ low - 1 + (abs n - abs high)
      | n < low = wrap' (low, high) $ high + 1 - (abs n - abs low)
      | otherwise = n

-- wrap' (-20, -10) -9 = -20

safeExtract :: (Default a, Ix i) => Grid i a -> a
safeExtract g = fromMaybe def $ grid g !? focus g

class Default a where
  def :: a

instance Default Bool where
  def = False
