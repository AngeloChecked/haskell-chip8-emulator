module Main (main) where

import qualified Graphics.Vty as T

import Display (loop, whiteDisplay) 
import Emulator (nextTick, EmulatorState(..))
import Timer (newOneSecondTimer)

initialState :: IO EmulatorState
initialState = do 
    timer <- newOneSecondTimer
    return $ EmulatorState whiteDisplay 0 (timer,0,0)

main :: IO ()
main = do
  vty <- T.mkVty T.defaultConfig
  state <- initialState

  loop vty state nextTick

  T.shutdown vty


   

