module Emulator (nextTick, EmulatorState (EmulatorState), displaySize, Display, DisplaySize) where

import Data.Array
import Timer (Timer)

type Coord = (Int, Int)

type Display = Array Coord Bool

data EmulatorState = EmulatorState {
                        display :: !Display,
                        cursor :: !Int,
                        speed :: !(Timer, Int, Int)
                     }
  deriving (Eq)

type DisplaySize = (Int,Int)

displaySize :: DisplaySize
displaySize = (64, 32)

nextTick :: EmulatorState -> EmulatorState
nextTick (EmulatorState previousDisplay pCursor s) =
  let (w, h) = displaySize
      cooFromCursor actualCursor =
        let x = (actualCursor `mod` w)
            y = (actualCursor `div` w)
         in (x, y)

      preCoord = cooFromCursor pCursor
      okCursor = if pCursor == (w -1) * (h -1) then 0 else pCursor
      newCoord = cooFromCursor (okCursor + 1)
      newDisplay = previousDisplay // [(preCoord, True), (newCoord, False)]
   in EmulatorState newDisplay (okCursor + 1) s