module Emulator (nextTick, EmulatorState (..), displaySize, Display, DisplaySize, Keyboard) where

import Data.Array
import Timer (Timer)
import qualified Graphics.Vty as T
import qualified Data.Map as M

type Coord = (Int, Int)

type Display = Array Coord Bool

type Keyboard = [Bool]

data EmulatorState = EmulatorState {
                        display :: !Display,
                        cursor :: !Int,
                        speed :: !(Timer, Int, Int),
                        keyboard :: !Keyboard
                     }
  deriving (Eq)


type DisplaySize = (Int,Int)

displaySize :: DisplaySize
displaySize = (64, 32)

keyboardKeys :: M.Map Char Int
keyboardKeys =
      M.fromList
        [ ('1',1), 
          ('2',2), 
          ('3',3), 
          ('4',12), 
          ('q',4), 
          ('w',5), 
          ('e',6), 
          ('r',13), 
          ('a',7), 
          ('s',8), 
          ('d',9), 
          ('f',14), 
          ('z',10), 
          ('x',0), 
          ('c',11), 
          ('v',15) 
        ]

resetKeyboard :: Keyboard -> Maybe T.Event -> Keyboard 
resetKeyboard k Nothing = k
resetKeyboard k (Just (T.EvKey (T.KChar c) _))  = maybe k (setKeyboard k 0) (M.lookup c keyboardKeys)
        where 
          setKeyboard :: Keyboard -> Int -> Int -> Keyboard
          setKeyboard [] _ _ = []
          setKeyboard (_:xs) i pressedIndex = (i == pressedIndex ) : setKeyboard xs (i+1) pressedIndex
resetKeyboard k (Just _) = k
      

nextTick :: Maybe T.Event -> EmulatorState -> EmulatorState
nextTick event (EmulatorState previousDisplay pCursor s k) =
  let (w, h) = displaySize
      cooFromCursor actualCursor =
        let x = (actualCursor `mod` w)
            y = (actualCursor `div` w)
         in (x, y)

      preCoord = cooFromCursor pCursor
      okCursor = if pCursor == (w -1) * (h -1) then 0 else pCursor
      newCoord = cooFromCursor (okCursor + 1)
      newDisplay = previousDisplay // [(preCoord, True), (newCoord, False)]

      newKeyboard = resetKeyboard k event
      
   in EmulatorState newDisplay (okCursor + 1) s newKeyboard    
  
  
