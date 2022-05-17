module Main where

import AppTypes
import HaskSweep
import Constants
import Draw
import Graphics.Gloss.Interface.IO.Game
import Inputs

windowDisplay :: Display
windowDisplay = InWindow "HaskSweeper" (windowSize, windowSize) (10, 10)

initialState :: World
initialState =
  World
    { _board = mkEmptyBoard boardSize
    }

-- Update
update :: Float -> World -> IO World
update _ = pure

-- Main
main :: IO ()
main = playIO windowDisplay (light orange) fps initialState draw inputHandler update
