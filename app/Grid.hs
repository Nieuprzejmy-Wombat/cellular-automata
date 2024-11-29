{-# LANGUAGE DeriveFunctor #-}

module Grid where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Array
import Data.Array.Base ((!?))
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
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
  duplicate g = g{grid = array (bounds $ grid g) . fmap (\s -> (s, g{focus = s})) . indices . grid $ g}

class Default a where
  def :: a

instance Default Bool where
  def = False

instance Default ThreeState where
  def = Off

neighboursMoore :: (Default a) => Grid (Int, Int) a -> [a]
neighboursMoore (Grid g (x, y)) = fmap (safeExtract . Grid g) [(i, j) | i <- [pred x, x, succ x], j <- [pred y, y, succ y], (i, j) /= (x, y)]

safeExtract :: (Ix s, Default a) => Grid s a -> a
safeExtract g = fromMaybe def $ grid g !? focus g

type Rule s a = Grid s a -> a

data Automaton s a = Automaton {pprint :: Grid s a -> IO (), rule :: Grid s a -> a, start :: Config -> Grid s a}

thirty :: Automaton Int Bool
thirty = Automaton _pprint _rule _start
 where
  neighbours g = [safeExtract g{focus = pred $ focus g}, safeExtract g{focus = succ $ focus g}]
  _pprint = putStr . fmap (bool '⬜' '⬛') . elems . grid
  _rule g = let [p, r] = neighbours g in p /= (extract g || r)
  _start cfg = Grid (array (0, width cfg) [(x, def) | x <- [0 .. width cfg]] `toggle` [width cfg `div` 2]) 0

conway :: Automaton (Int, Int) Bool
conway = Automaton _pprint _rule _start
 where
  _pprint g = putStr . concat $ [[bool '⬜' '⬛' (grid g ! (x, y)) | x <- [x0 .. x1]] ++ "\n" | y <- [y0 .. y1]]
   where
    ((x0, y0), (x1, y1)) = bounds . grid $ g
  _rule g = case (extract g, length . filter id . neighboursMoore $ g) of
    (True, x) | x < 2 -> False
    (True, x) | x == 2 || x == 3 -> True
    (True, x) | x > 3 -> False
    (False, x) | x == 3 -> True
    (b, _) -> b
  _start cfg = Grid arr (0, 0)
   where
    arr = array ((0, 0), (width cfg, height cfg)) [((x, y), def) | x <- [0 .. width cfg], y <- [0 .. height cfg]] `toggle` [(midx, midy), (midx + 1, midy), (midx + 1, midy + 1), (midx + 1, midy - 1), (midx + 3, midy), (midx + 2, midy - 1)]
    midx = width cfg `div` 2
    midy = height cfg `div` 2

data ThreeState = On | Dying | Off
  deriving (Eq)

brian :: Automaton (Int, Int) ThreeState
brian = Automaton _pprint _rule _start
 where
  _pprint g = sequence_ $ [traverse helper [grid g ! (x, y) | x <- [x0 .. x1]] *> putStr "\n" | y <- [y0 .. y1]]
   where
    ((x0, y0), (x1, y1)) = bounds . grid $ g
    helper On = putStr "⬛"
    helper Dying = setSGR [SetColor Foreground Vivid Red] *> putStr "⬛" *> setSGR []
    helper Off = putStr "⬜"
  _rule g = case extract g of
    On -> Dying
    Dying -> Off
    Off | length (filter (== On) $ neighboursMoore g) == 2 -> On
    Off -> Off
  _start cfg = Grid arr (0, 0)
   where
    arr = array ((0, 0), (width cfg, height cfg)) [((x, y), def) | x <- [0 .. width cfg], y <- [0 .. height cfg]] `toggleBrian` [(midx, midy), (midx + 1, midy), (midx + 1, midy + 1), (midx + 1, midy + 2), (midx, midy - 1), (midx - 1, midy), (midx - 2, midy), (midx - 1, midy - 1)]
    midx = width cfg `div` 2
    midy = height cfg `div` 2

toggle :: (Ix s) => Array s Bool -> [s] -> Array s Bool
toggle arr points = arr // [(p, True) | p <- points]

toggleBrian :: (Ix i) => Array i ThreeState -> [i] -> Array i ThreeState
toggleBrian arr points = arr // [(p, On) | p <- points]

startBrian :: Config -> Grid (Int, Int) ThreeState
startBrian cfg = Grid arr (0, 0)
 where
  arr = array ((0, 0), (width cfg, height cfg)) [((x, y), def) | x <- [0 .. width cfg], y <- [0 .. height cfg]] `toggleBrian` [(midx, midy), (midx + 1, midy), (midx + 1, midy + 1), (midx + 1, midy + 2), (midx, midy - 1), (midx - 1, midy), (midx - 2, midy), (midx - 1, midy - 1)]
  midx = width cfg `div` 2
  midy = height cfg `div` 2

loop :: (Ix s) => Config -> Automaton s a -> IO ()
loop cfg auto = void $ saveCursor *> clearScreen *> loop' (start auto cfg)
 where
  loop' g = pprint auto g *> hFlush stdout *> restoreCursor *> clearScreen *> threadDelay (time cfg * 1000) *> loop' (extend (rule auto) g)
