{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Display (loop, baseDisplay, whiteDisplay) where

import Control.Monad
import Data.Array (array, (!))
import qualified Data.Map as M 
import Emulator (Display, DisplaySize, EmulatorState (EmulatorState, cursor, display, keyboard, speed), Keyboard, displaySize)
import qualified Graphics.Vty as T
import Timer (nextSecond)

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
applyState vty EmulatorState {display, speed, keyboard} = do
  let (_, _, lastSecondTick) = speed
  let keypad = makeKeypad keyboard
  let displayImage = buildDisplayImage displaySize display
  let displayPicture = T.picForLayers [exitMessage, tpsMessage lastSecondTick, keypad, displayImage]
  T.update vty displayPicture

makeKeypad :: Keyboard -> T.Image
makeKeypad keyboard = do
    let (_, height) = displaySize
    let mapping =
          M.fromList
            [ (1,'1'),
              (2,'2'),
              (3,'3'),
              (12,'4'),
              (4,'q'),
              (5,'w'),
              (6,'e'),
              (13,'r'),
              (7,'a'),
              (8,'s'),
              (9,'d'),
              (14,'f'),
              (10,'z'),
              (0,'x'),
              (11,'c'),
              (15,'v')
            ]
    let keypadForm = [ 
                       [1,  2, 3 , 12],
                       [4,  5, 6 , 13],
                       [7,  8, 9 , 14],
                       [10, 0, 11, 15]
                     ]

    let toImage i = let 
             pressed = (keyboard !! i)
             key = (M.!) mapping i
          in keyForKeypad pressed key

    let keypad = T.vertCat $ T.horizCat <$> (map . map) toImage keypadForm
    T.translate 0 (height+2) keypad


keyForKeypad :: Bool -> Char -> T.Image
keyForKeypad True c  = T.char (T.defAttr `T.withForeColor` T.black `T.withBackColor` T.white) c
keyForKeypad False c = T.char (T.defAttr `T.withForeColor` T.white `T.withBackColor` T.black) c

loop :: T.Vty -> EmulatorState -> (Maybe T.Event -> EmulatorState -> EmulatorState) -> IO ()
loop vty state nextTick = do
  let EmulatorState {speed} = state
  let (timer, nTick, lastTick) = speed

  (sPassed, newTimer) <- nextSecond timer
  let tickToShow = if sPassed then nTick else lastTick
  let nTickReset = if sPassed then 0 else nTick + 1

  event <- T.nextEventNonblocking vty
  applyState vty state

  let EmulatorState {display, cursor, keyboard} = nextTick event state

  let newState = EmulatorState display cursor (newTimer, nTickReset, tickToShow) keyboard

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
whiteDisplay = baseDisplay displaySize

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
