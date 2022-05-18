{-# LANGUAGE TemplateHaskell #-}

module AppTypes
  ( World,
    worldSelectedCell,
    worldMarked,
    worldGameStatus,
    worldBoard,
    mkWorld,
  )
where

import Control.Lens
import Data.Set
import HaskSweep

data World = World
  { _worldSelectedCell :: Maybe (Int, Int),
    _worldMarked :: Set (Int, Int),
    _worldGameStatus :: GameStatus,
    _worldBoard :: Board
  }

makeLenses ''World

mkWorld :: Board -> World
mkWorld = World Nothing (fromList []) Ok
