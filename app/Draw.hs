module Draw
  ( draw,
  )
where

import AppTypes
import Constants
import Control.Lens
import Data.Maybe
import Graphics.Gloss
import HaskSweep

draw :: World -> IO Picture
draw w = pure . translate (- windowMiddle) (- windowMiddle) $ everything
  where
    cells = pictures $ drawCell <$> getCells (w ^. board)
    selection = drawSelectedCell $ w ^. selectedCell
    everything = pictures [cells, selection]

translateToPos :: (Int, Int) -> Picture -> Picture
translateToPos (i, j) = translate (cellSize * fromIntegral i) (cellSize * fromIntegral j)

drawCell :: ((Int, Int), Cell) -> Picture
drawCell (p, cell) = translateToPos p $ pictures $ cellToPic cell
  where
    cellGrid = color black $ rectangleWire cellSize cellSize
    cellToPic c =
      cellGrid :
      [ (c ^. cellState) & \case
          HiddenCell -> color black $ rectangleWire (cellSize * 0.5) (cellSize * 0.5)
          VisibleCell ->
            scale 0.75 0.75 . translate (-cellSize * 0.35) (-cellSize * 0.5) $
              (c ^. cellType) & \case
                MinedCell -> text "X"
                EmptyCell x -> if x == 0 then blank else text $ show x
      ]

drawSelectedCell :: Maybe (Int, Int) -> Picture
drawSelectedCell maybePos = fromMaybe blank $ maybePos <&> (\pos -> translateToPos pos $ color red $ rectangleWire (cellSize * 0.5) (cellSize * 0.5))
