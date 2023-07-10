module Timer (newOneSecondTimer, nextSecond, Timer) where 

import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe (isJust)
import Control.Monad (when)
import Data.Either (isLeft)


type Timer = (TMVar (), ThreadId)

nextSecond :: Timer -> IO (Bool, Timer)
nextSecond timer = do
    secondPassed <- isTimerFinished timer
    let okTimer = if secondPassed then newOneSecondTimer else pure timer    
    (,) secondPassed <$> okTimer

isTimerFinished :: Timer -> IO Bool
isTimerFinished (timer, _) = do 
    x <- atomically $ tryReadTMVar timer
    return $ isJust x

newOneSecondTimer :: IO Timer
newOneSecondTimer = do
    timer <- atomically newEmptyTMVar
    tId <- flip forkFinally (\e -> when (isLeft e) (print $ "thread not finished correctly: " ++ show e)) $ do
        threadDelay (1*1000000)
        atomically $ do
            putTMVar timer ()
    return (timer, tId)
