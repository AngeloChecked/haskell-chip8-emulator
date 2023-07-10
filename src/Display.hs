module Display (loop, baseDisplay, whiteDisplay) where

import Data.Array ((!), array)
import qualified Graphics.Vty as T
import Control.Monad
import Emulator(EmulatorState (EmulatorState), displaySize, Display, DisplaySize)
import Timer (nextSecond, newOneSecondTimer, Timer)

-- possible refactor:
-- import Control.Monad.RWS
-- type Emulator = RWST T.Vty () EmulatorState IO
--  (finalState, ()) <- execRWST loop vty state

exitMessage :: T.Image
exitMessage = do
  let (_, height) = displaySize
  let exitText = T.string (T.defAttr `T.withForeColor` T.white `T.withBackColor` T.black) "Press ESC to exit."
  T.translate 0 height exitText

tpsMessage :: Int -> T.Image
tpsMessage n = do
  let (width, height) = displaySize
  let msg = show n ++ " tick per second"
  let tpsText = T.string (T.defAttr `T.withForeColor` T.white `T.withBackColor` T.black) msg
  T.translate (width - length msg) height tpsText

applyState :: T.Vty -> EmulatorState -> IO ()
applyState vty (EmulatorState display _ (_, _, lastSecondTick)) = do 
  let displayImage = buildDisplayImage displaySize display
  let displayPicture = T.picForLayers [exitMessage, tpsMessage lastSecondTick, displayImage]
  T.update vty displayPicture
  
loop :: T.Vty -> EmulatorState -> (EmulatorState -> EmulatorState) -> IO ()
loop vty state nextTick = do

    let (EmulatorState _ _ (timer, nTick, lastTick)) = state
    (sPassed, newTimer) <- nextSecond timer
    let tickToShow = if sPassed then nTick else lastTick
    let nTickReset = if sPassed then 0 else nTick+1

    applyState vty state

    event <- T.nextEventNonblocking vty

    let (EmulatorState display cursor _) = nextTick state
    let newState = EmulatorState display cursor (newTimer, nTickReset, tickToShow)

    T.refresh vty
    unless (isExit event) (loop vty newState nextTick)

  where 
      isExit (Just (T.EvKey T.KEsc [])) = True
      isExit _ = False

white, black :: T.Image
white = T.char (T.defAttr `T.withBackColor` T.white) ' '
black = T.char (T.defAttr `T.withBackColor` T.black) ' '

imageForDisplay :: Bool -> T.Image
imageForDisplay True = white
imageForDisplay False = black

baseDisplay :: DisplaySize -> Display
baseDisplay (width, height) =
  array
    ((0, 0), (width -1, height -1))
    [((x, y), True) | x <- [0 .. width -1], y <- [0 .. height -1]]

whiteDisplay :: Display
whiteDisplay =  baseDisplay displaySize

buildDisplayImage :: DisplaySize -> Display -> T.Image
buildDisplayImage (displayWidth, displayHeight) display =
  T.vertCat
    [ displayRow
      | y <- [0 .. displayHeight -1],
        let displayRow =
              T.horizCat
                [ i | x <- [0 .. displayWidth -1], let i = imageForDisplay (display ! (x, y))
                ]
    ]
