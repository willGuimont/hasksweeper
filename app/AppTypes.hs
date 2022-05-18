{-# LANGUAGE TemplateHaskell #-}

module AppTypes
  ( World,
    worldSelectedCell,
    worldMarked,
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
    _worldBoard :: Board
  }

makeLenses ''World

mkWorld :: Board -> World
mkWorld = World Nothing (fromList [])
