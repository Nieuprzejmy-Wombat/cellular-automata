{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Automata where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Array
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Grid
import Options
import System.Console.ANSI
import System.IO

data Automaton s a = Automaton {pprint :: Grid s a -> IO (), rule :: Grid s a -> a, start :: s -> Grid s a}

defaultAutomaton :: (Coord s, Show a, Default a) => Automaton s a
defaultAutomaton = Automaton (traverse_ print) extract $ defaultGrid . centered

toggle :: (Ix s) => Grid s Bool -> [s] -> Grid s Bool
toggle g points = g {grid = grid g // [(p, True) | p <- points]}

neighbourValues :: (Coord s) => Grid s a -> [a]
neighbourValues g = (\n -> extract g {focus = n}) <$> neighbours (focus g)

conway :: Automaton (Int, Int) Bool
conway = (defaultAutomaton @(Int, Int) @Bool) {pprint = _pprint, rule = _rule, start = (`toggle` coords) . start defaultAutomaton}
  where
    _pprint g = putStr . concat $ [[bool '⬜' '⬛' (grid g ! (x, y)) | x <- [x0 .. x1]] ++ "\n" | y <- [y0 .. y1]]
      where
        ((x0, y0), (x1, y1)) = bounds . grid $ g
    _rule g = case (extract g, length . filter id . neighbourValues $ g) of
      (True, x) | x < 2 -> False
      (True, x) | x == 2 || x == 3 -> True
      (True, x) | x > 3 -> False
      (False, x) | x == 3 -> True
      (b, _) -> b
    coords = [(0, 0), (1, 0), (1, 1), (1, -1), (3, 0), (2, -1)]

data ThreeState = On | Dying | Off
  deriving (Eq, Show)

instance Default ThreeState where
  def = Off

toggleBrian :: (Ix s) => Grid s ThreeState -> [s] -> Grid s ThreeState
toggleBrian g points = g {grid = grid g // [(p, On) | p <- points]}

brian :: Automaton (Int, Int) ThreeState
brian = (defaultAutomaton @(Int, Int) @ThreeState) {pprint = _pprint, rule = _rule, start = (`toggleBrian` coords) . start defaultAutomaton}
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
      Off | length (filter (== On) $ neighbourValues g) == 2 -> On
      Off -> Off
    coords = [(0, 0), (1, 0), (1, 1), (1, 2), (0, -1), (-1, 0), (-2, 0), (-1, -1)]

loop :: (Coord s) => s -> Config -> Automaton s a -> IO ()
loop size cfg auto = void $ saveCursor *> clearScreen *> loop' (start auto size)
  where
    loop' g = pprint auto g *> hFlush stdout *> restoreCursor *> clearScreen *> threadDelay (delta cfg * 1000) *> loop' (extend (rule auto) g)
