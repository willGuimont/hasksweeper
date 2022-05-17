{-# LANGUAGE TemplateHaskell #-}

module AppTypes
  ( World,
    mkWorld,
    getBoard,
  )
where

import Control.Lens
import HaskSweep

newtype World = World {_board :: Board}

makeLenses ''World

mkWorld :: Board -> World
mkWorld = World

getBoard :: World -> Board
getBoard = view board
