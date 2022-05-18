module Inputs
  ( inputHandler,
  )
where

import AppTypes
import Constants
import Control.Lens
import Graphics.Gloss.Interface.IO.Game
import HaskSweep

inputHandler :: Event -> World -> IO World
inputHandler (EventMotion pos) w = pure (w & selectedCell .~ posToBoardIndex pos)
inputHandler (EventKey (MouseButton LeftButton) Down _ pos) w =
  -- TODO handle game status
  let (_, newBoard) = maybe (Ok, w ^. board) (revealCell (w ^. board)) (posToBoardIndex pos)
   in pure (w & board .~ newBoard)
inputHandler _ w = pure w

posToBoardIndex :: Point -> Maybe (Int, Int)
posToBoardIndex (x, y) = toMaybe $ bimap round round ((x + windowMiddle) / cellSize, (y + windowMiddle) / cellSize)
  where
    inBound i = 1 <= i && i <= boardSize
    isValid (i, j) = inBound i && inBound j
    toMaybe p = if isValid p then Just p else Nothing
