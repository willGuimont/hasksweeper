{-# LANGUAGE TemplateHaskell #-}

module AppTypes
  ( World,
    selectedCell,
    board,
    mkWorld,
  )
where

import Control.Lens
import HaskSweep

data World = World
  { _selectedCell :: Maybe (Int, Int),
    _board :: Board
  }

makeLenses ''World

mkWorld :: Board -> World
mkWorld = World Nothing
