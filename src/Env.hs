{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Env
    ( Env(..)
    , SocialM
    , runSocialM
    ) where

import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, readMVar, modifyMVar, modifyMVar_, putMVar)
import qualified Data.Map as Map
import System.IO (Handle, hPutStrLn)
import System.Random (randomRIO)
import qualified Data.Time (getCurrentTime)
import System.Timeout (timeout)
import Control.Exception (try, IOException)

import Types
import Interfaces

-- | Global Environment (Infrastructure)
data Env = Env
    { envCount     :: MVar Int                -- ^ This is for Counter 
    , envStats     :: MVar (Map.Map Topic Int) -- ^ This is for Map for topics
    , envDone      :: MVar ()                 -- ^ This is for Stop signal 
    , envLog       :: MVar Handle             -- ^ This is for File handle for logging
    }

-- | This is SocialM monad for getting shared Env
type SocialM = ReaderT Env IO

-- | Here we are running SocialM monad
runSocialM :: Env -> SocialM a -> IO a
runSocialM = flip runReaderT

-- | This connects the Interface to the respectiveImplementation
instance SocialLife SocialM where
    sendMsg recipient msg = do
        -- Timeout for preventing deadlock
        -- Trying  to send for 0.5s. If it gets blocked, then we will give up.
        result <- liftIO $ timeout 500000 $ modifyMVar_ (inbox recipient) (\msgs -> return (msg : msgs))
        case result of
             Just _  -> return () -- Sent successfully
             Nothing -> logStr $ "Failed to send message to " ++ username recipient ++ " (Timeout)"
        
    isBatteryAlive u = do
        batt <- liftIO $ readMVar (battery u)
        return (batt > 0)
        
    drainBattery u amount = 
        liftIO $ modifyMVar (battery u) $ \b -> do
            let nb = max 0 (b - amount)
            return (nb, nb)
            
    randInt range = liftIO $ randomRIO range
    
    logStr msg = do
        logVar <- asks envLog
        -- 3.  Logging
        -- If logging fails, catch it and It will print to stderr rather than crashing thread.
        liftIO $ do
            res <- try $ modifyMVar_ logVar $ \h -> do
                hPutStrLn h msg
                return h
            case res of
                Left e -> putStrLn $ "SafeLog Fail: " ++ show (e :: IOException)
                Right _ -> return ()
            
    checkLimit = do
        counterVar <- asks envCount
        liftIO $ modifyMVar counterVar $ \val -> do
            if val < 100
                then return (val + 1, (True, val + 1 == 100))
                else return (val, (False, False))
                
    signalEnd = do
        doneVar <- asks envDone
        liftIO $ putMVar doneVar ()
        
    recordStats topic sender recipient = do
        -- 1. Here the sender sent count
        liftIO $ modifyMVar_ (sentCount sender) (\c -> return (c + 1))
        -- 2. Here the recipient received count
        liftIO $ modifyMVar_ (messageCount recipient) (\c -> return (c + 1))
        -- 3. Here the global topic stats
        statsVar <- asks envStats
        liftIO $ modifyMVar_ statsVar (\m -> return (Map.insertWith (+) topic 1 m))
        
    sleep us = liftIO $ threadDelay us
        
    getNow = do
        t <- liftIO $ Data.Time.getCurrentTime
        return (show t)
