module Draw where

import AppTypes
import Control.Lens
import HaskSweep
import Graphics.Gloss

draw :: World -> IO Picture
draw w = pure . pictures $ undefined
