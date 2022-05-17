{-# LANGUAGE TemplateHaskell #-}

module HaskSweep
  ( Board,
    mkEmptyBoard,
    addRandomMines,
    getCells,
  )
where

import Control.Lens
import Data.Array
import Data.Maybe

data CellType = EmptyCell Int | MinedCell deriving (Show)

data CellState = HiddenCell | VisibleCell

data Cell = Cell
  { _cellType :: CellType,
    _cellState :: CellState
  }

makeLenses ''Cell

data Board = Board
  { _boardCells :: Array (Int, Int) Cell,
    _boardSize :: Int
  }

makeLenses ''Board

defaultCell :: Cell
defaultCell =
  Cell
    { _cellType = EmptyCell 0,
      _cellState = HiddenCell
    }

mkEmptyBoard :: Int -> Board
mkEmptyBoard s =
  Board
    { _boardCells = array ((1, 1), (s, s)) ([((i, j), defaultCell) | i <- [1 .. s], j <- [1 .. s]]),
      _boardSize = s
    }

getCells :: Board -> [((Int, Int), Cell)]
getCells = undefined

-- TODO put `n` random mines in the board
addRandomMines :: Int -> Board -> IO Board
addRandomMines n = return . addMineAt (3, 3)
  where
    addMineCloseToCell (EmptyCell x) = EmptyCell (x + 1)
    addMineCloseToCell MinedCell = MinedCell
    -- TODO simplify this using monad
    addMineNeighborhood p b = foldl (\b' p' -> b' & boardCells . ix p' . cellType %~ addMineCloseToCell) b (getNeighborhoodIndices p)
    addMineAt p = addMineNeighborhood p . (boardCells . ix p . cellType .~ MinedCell)

neighborDelta :: [(Int, Int)]
neighborDelta = [(i, j) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]

getNeighborhoodIndices :: (Int, Int) -> [(Int, Int)]
getNeighborhoodIndices (i, j) = bimap (i +) (j +) <$> neighborDelta

getNeighbors :: Board -> (Int, Int) -> [Cell]
getNeighbors b p = catMaybes $ (\x -> b ^? (boardCells . ix x)) <$> getNeighborhoodIndices p

-- TODO
--revealCell :: Board -> (Int, Int) -> Either GameStatus Board
--revealCell = undefined
