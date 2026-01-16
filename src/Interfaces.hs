module Interfaces where

import Types

-- | This defines SocialLife interface for SocialM monad
class Monad m => SocialLife m where
    -- | It helps toSend a message to another user
    sendMsg :: User -> Message -> m ()
    
    -- | Checking if battery is alive
    isBatteryAlive :: User -> m Bool
    
    -- | Reduce my battery
    drainBattery :: User -> Int -> m Int
    
    -- | Get random number
    randInt :: (Int, Int) -> m Int
    
    -- | Logging a string
    logStr :: String -> m ()
    
    -- | Checking global limit ,This will return shouldStop, limitReached
    checkLimit :: m (Bool, Bool)
    
    -- | Signal that we are done
    signalEnd :: m ()
    
    -- | Update the Stats
    recordStats :: Topic -> User -> User -> m ()
    
    -- | It defines Sleep for microseconds
    sleep :: Int -> m ()
    
    -- | It will get  current time as String
    getNow :: m String
