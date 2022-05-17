module Inputs
  ( inputHandler,
  )
where

import AppTypes
import Graphics.Gloss.Interface.IO.Game

inputHandler :: Event -> World -> IO World
inputHandler e w = pure w
