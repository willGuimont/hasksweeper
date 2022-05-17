module Draw
  ( draw,
  )
where

import AppTypes
import Constants
import Control.Lens
import Graphics.Gloss
import HaskSweep

draw :: World -> IO Picture
draw w = pure . translate (- windowMiddle) (- windowMiddle) . pictures $ drawCell <$> getCells (getBoard w)

drawCell :: ((Int, Int), Cell) -> Picture
drawCell ((i, j), cell) = translate (cellSize * (fromIntegral i - 0.5)) (cellSize * (fromIntegral j - 0.5)) $ text $ cellToString cell
  where
    cellToString c =
      (c ^. cellType) & \case
        MinedCell -> "x"
        EmptyCell x -> show x
