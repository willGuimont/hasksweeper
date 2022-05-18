module Main
  ( main,
  )
where

import AppTypes
import Constants
import Draw
import Graphics.Gloss.Interface.IO.Game
import HaskSweep
import Inputs

windowDisplay :: Display
windowDisplay = InWindow "HaskSweeper" (windowSize, windowSize) (10, 10)

-- Update
update :: Float -> World -> IO World
update _ = pure

-- Main
main :: IO ()
main = do
  initBoard <- addRandomMines numMines $ mkEmptyBoard boardSize
  let initialState = mkWorld initBoard
  playIO windowDisplay (light orange) fps initialState draw inputHandler update
