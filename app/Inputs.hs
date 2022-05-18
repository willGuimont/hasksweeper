module Inputs
  ( inputHandler,
  )
where

import AppTypes
import Constants
import Control.Lens
import Data.Set
import Graphics.Gloss.Interface.IO.Game
import HaskSweep

inputHandler :: Event -> World -> IO World
inputHandler (EventMotion pos) w = pure (w & worldSelectedCell .~ posToBoardIndex pos)
inputHandler (EventKey (MouseButton LeftButton) Down _ pos) w =
  -- TODO handle game status
  let (gameStatus, newBoard) = maybe (Ok, w ^. worldBoard) (revealCell (w ^. worldBoard)) (posToBoardIndex pos)
   in print gameStatus >> pure (w & worldBoard .~ newBoard)
inputHandler (EventKey (MouseButton RightButton) Down _ pos) w =
  let boardPos = posToBoardIndex pos
      marked = w ^. worldMarked
      newMarked = case boardPos of
        Nothing -> marked
        Just p ->
          if p `member` marked
            then delete p marked
            else insert p marked
   in pure . (worldMarked .~ newMarked) $ w
inputHandler _ w = pure w

posToBoardIndex :: Point -> Maybe (Int, Int)
posToBoardIndex (x, y) = toMaybe $ bimap round round ((x + windowMiddle) / cellSize, (y + windowMiddle) / cellSize)
  where
    inBound i = 1 <= i && i <= boardSize
    isValid (i, j) = inBound i && inBound j
    toMaybe p = if isValid p then Just p else Nothing
