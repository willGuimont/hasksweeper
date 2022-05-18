{-# LANGUAGE TemplateHaskell #-}

module HaskSweep
  ( Board,
    Cell,
    CellType (..),
    cellType,
    CellState (..),
    cellState,
    mkEmptyBoard,
    addRandomMines,
    getCells,
    GameStatus (..),
    revealCell,
  )
where

import Control.Lens
import Control.Monad.State
import Data.Array
import Data.Maybe

-- Cells
data CellType = EmptyCell Int | MinedCell deriving (Eq)

data CellState = HiddenCell | VisibleCell deriving (Eq)

data Cell = Cell
  { _cellType :: CellType,
    _cellState :: CellState
  }

makeLenses ''Cell

isEmpty :: CellType -> Bool
isEmpty MinedCell = False
isEmpty (EmptyCell _) = True

-- Board
data Board = Board
  { _boardCells :: Array (Int, Int) Cell,
    _boardSize :: Int
  }

makeLenses ''Board

mkEmptyBoard :: Int -> Board
mkEmptyBoard s =
  Board
    { _boardCells = array ((1, 1), (s, s)) ([((i, j), defaultCell) | i <- [1 .. s], j <- [1 .. s]]),
      _boardSize = s
    }
  where
    defaultCell =
      Cell
        { _cellType = EmptyCell 0,
          _cellState = HiddenCell
        }

getCells :: Board -> [((Int, Int), Cell)]
getCells b = zip (Data.Array.indices cs) (elems cs)
  where
    cs = b ^. boardCells

-- TODO put `n` random mines in the board
addRandomMines :: Int -> Board -> IO Board
addRandomMines n = return . addMineAt (3, 3) . addMineAt (4, 4) . addMineAt (3, 4)
  where
    addMineCloseToCell (EmptyCell x) = EmptyCell (x + 1)
    addMineCloseToCell MinedCell = MinedCell
    addMineNeighborhood p b = foldl (\b' p' -> b' & boardCells . ix p' . cellType %~ addMineCloseToCell) b (getNeighborhoodIndices p)
    addMineAt p = addMineNeighborhood p . (boardCells . ix p . cellType .~ MinedCell)

-- Reveal cell
data GameStatus = Won | Lost | Ok

type VisitableCells = Array (Int, Int) (Bool, Cell)

revealCell :: Board -> (Int, Int) -> (GameStatus, Board)
revealCell b p = (status, newBoard)
  where
    visitableCells = (False,) <$> (b ^. boardCells)
    visitedCells = evalState (revealCells visitableCells p) visitableCells
    newCells = snd <$> visitedCells
    newBoard = b & boardCells .~ newCells
    numMines = sum $ fromEnum . not . isEmpty . _cellType . snd <$> getCells newBoard
    numVisible = sum $ fromEnum . (== VisibleCell) . _cellState . snd <$> getCells newBoard
    numCells = (b ^. boardSize) ^ 2
    status = Ok

neighborDelta :: [(Int, Int)]
neighborDelta = [(i, j) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0), abs j + abs i == 1]

getNeighborhoodIndices :: (Int, Int) -> [(Int, Int)]
getNeighborhoodIndices (i, j) = bimap (i +) (j +) <$> neighborDelta

getNeighbors :: (Int, Int) -> Array (Int, Int) Cell -> [((Int, Int), Cell)]
getNeighbors p b = catMaybes $ (\x -> (x,) <$> b ^? ix x) <$> getNeighborhoodIndices p

revealCells :: VisitableCells -> (Int, Int) -> State VisitableCells VisitableCells
revealCells b p = do
  visitCell b
  get >>= \b'' -> foldM update b'' visitableNeighbors
  where
    neighbors = getNeighbors p $ snd <$> b
    isCellVisitable p' = not (fromMaybe False (b ^? (ix p' . _1))) && maybe False isEmpty (b ^? (ix p' . _2 . cellType))
    visitableNeighbors = filter isCellVisitable $ fst <$> neighbors
    visitCell b' = put $ (b' & ix p . _2 . cellState .~ VisibleCell) & (ix p . _1 .~ True)
    update b' p' = do
      visitCell b'
      get >>= \b'' -> revealCells b'' p' >>= put >> get
