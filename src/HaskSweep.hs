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
import System.Random.Shuffle

-- Cells
data CellType = EmptyCell Int | MinedCell deriving (Show, Eq)

data CellState = HiddenCell | VisibleCell deriving (Show, Eq)

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

addRandomMines :: Int -> Board -> IO Board
addRandomMines n b = do
  foldl addMineAt b <$> randomPosition
  where
    randomPosition = do
      let s = b ^. boardSize
      let allPositions = [(i, j) | i <- [1 .. s], j <- [1 .. s]]
      shuffled <- shuffleM allPositions
      return . take n $ shuffled
    addMineCloseToCell (EmptyCell x) = EmptyCell (x + 1)
    addMineCloseToCell MinedCell = MinedCell
    addMineNeighborhood p b' = foldl (\b'' p' -> b'' & boardCells . ix p' . cellType %~ addMineCloseToCell) b' (getNeighborhoodIndices p)
    addMineAt b' p = addMineNeighborhood p . (boardCells . ix p . cellType .~ MinedCell) $ b'

-- Reveal cell
data GameStatus = Won | Lost | Ok deriving (Show, Eq)

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
    numCells = (b ^. boardSize) ^ (2 :: Int)
    revealedCell = b ^? (boardCells . ix p . cellType)
    status
      | numVisible + numMines == numCells = Won
      | revealedCell == Just MinedCell = Lost
      | otherwise = Ok

neighborDelta :: [(Int, Int)]
neighborDelta = [(i, j) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]

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
    revealedCell = fromMaybe (EmptyCell 0) (b ^? (ix p . _2 . cellType))
    visitableNeighbors = if revealedCell == EmptyCell 0 then filter isCellVisitable $ fst <$> neighbors else []
    visitCell b' = put $ (b' & ix p . _2 . cellState .~ VisibleCell) & (ix p . _1 .~ True)
    update _ p' = do
      get >>= \b'' -> revealCells b'' p' >>= put >> get
