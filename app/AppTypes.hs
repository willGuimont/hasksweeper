{-# LANGUAGE TemplateHaskell #-}

module AppTypes where

import HaskSweep
import Control.Lens

newtype World = World {_board :: Board}

makeLenses ''World
