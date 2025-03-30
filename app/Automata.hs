{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Automata where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Array
import Data.Bool (bool)
import Grid
import Options
import System.Console.ANSI
import System.IO
import Vec

neighboursMoore :: (Default a) => Grid (Vec n Int) a -> [a]
neighboursMoore (Grid g f) = fmap (safeExtract . Grid g) [i | i <- range (pred <$> f, succ <$> f), i /= f]

data Automaton s a = Automaton {pprint :: Grid s a -> IO (), rule :: Grid s a -> a, start :: s -> Grid s a}

defaultAutomaton :: (Show a, Default a) => Automaton (Vec n Int) a
defaultAutomaton = Automaton print extract $ \cfg -> defaultGrid (negate . (`div` 2) <$> dimensions, (`div` 2) <$> dimensions)

toggle :: (Ix s) => Grid s Bool -> [s] -> Grid s Bool
toggle g points = g {grid = grid g // [(p, True) | p <- points]}

conway :: Automaton (Int, Int) Bool
conway = (defaultAutomaton @Bool) {pprint = _pprint, rule = _rule, start = (`toggle` coords) . start defaultAutomaton}
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
    coords = [(0, 0), (1, 0), (1, 1), (1, -1), (3, 0), (2, -1)]

data ThreeState = On | Dying | Off
  deriving (Eq, Show)

instance Default ThreeState where
  def = Off

toggleBrian :: (Ix s) => Grid s ThreeState -> [s] -> Grid s ThreeState
toggleBrian g points = g {grid = grid g // [(p, On) | p <- points]}

brian :: Automaton (Int, Int) ThreeState
brian = (defaultAutomaton @ThreeState) {pprint = _pprint, rule = _rule, start = (`toggleBrian` coords) . start defaultAutomaton}
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
    coords = [(0, 0), (1, 0), (1, 1), (1, 2), (0, -1), (-1, 0), (-2, 0), (-1, -1)]

loop :: (Ix s) => Config 2 -> Automaton s a -> IO ()
loop cfg auto = void $ saveCursor *> clearScreen *> loop' (start auto cfg)
  where
    loop' g = pprint auto g *> hFlush stdout *> restoreCursor *> clearScreen *> threadDelay (time cfg * 1000) *> loop' (extend (rule auto) g)
