module Constants
  ( fps,
    windowSize,
    windowMiddle,
    boardSize,
    numMines,
    cellSize,
  )
where

fps :: Int
fps = 20

windowSize :: Int
windowSize = 1000

windowMiddle :: Float
windowMiddle = fromIntegral windowSize / 2

boardSize :: Int
boardSize = 8

numMines :: Int
numMines = 10

cellSize :: Float
cellSize = 110
