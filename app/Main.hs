module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, takeMVar, putMVar, newEmptyMVar)
import Control.Monad (forM, forM_)
import Control.Exception (try, catch, IOException, SomeException, bracket)
import qualified Data.Map as Map
import Data.List (sortBy, intercalate, replicate)
import Data.Function (on)
import Data.List (sortBy, intercalate, replicate)
import Data.Function (on)
import System.IO (withFile, IOMode(..), hPutStrLn, hSetBuffering, BufferMode(..))
import Text.Printf (printf)
import Env
import Types
import User (simUser)
import Control.Parallel.Strategies (parMap, rpar)

import System.Random (randomRIO)

main :: IO ()
main = do
    printBoxed "Application \"social network\" Staring -> \n\n note that: If the user's battery drains to 0%, User cannot send any more messages."
    
    -- 1. Initialize Global State
    count <- newMVar 0
    stats <- newMVar Map.empty
    done <- newEmptyMVar

    -- 2. Below I am creating users
    users <- forM [1..10] $ \i -> 
        User ("User" ++ show i) <$> newMVar [] <*> newMVar 0 <*> newMVar 0 <*> (randomRIO (10, 100) >>= newMVar)

    -- 3. Below I am opening log file using try catch block
    result <- try $ withFile "messages.log" WriteMode $ \h -> do
        hSetBuffering h LineBuffering
        logVar <- newMVar h
        
        -- 4. Constructing Environment
        let env = Env { envCount = count
                      , envStats  = stats
                      , envDone  = done
                      , envLog   = logVar
                      }
    
        -- 5. Spawn Threads using SocialM 
        -- Below I am running the simulation in a forked thread
        forM_ users $ \u -> forkIO $ 
            catch (runSocialM env (simUser u users))
                  (\e -> putStrLn $ "Error in thread " ++ username u ++ ": " ++ show (e :: SomeException))
        
        
        takeMVar done
        
    -- Below I am handling the errors of log file
    case result of
        Left e -> putStrLn $ "Error handling log file: " ++ show (e :: IOException)
        Right _ -> do
            printBoxed "Simulation limit reached (100 messages)."
            
            -- 6. Below I am gathering results
            putStrLn ""
            putStrLn "--- Stats of Users ---"
            finalUsers <- forM users $ \u -> do
                s <- readMVar (sentCount u)
                r <- readMVar (messageCount u)
                b <- readMVar (battery u)
                return [username u, show s, show r, show b ++ "%"]
                
            printTable ["User", "Sent", "Received", "Battery Left"] finalUsers

            putStrLn ""
            putStrLn "--- Analysis the Trending Topics  (This is extension of the project) ---"
            topics <- readMVar stats
            let sorted = sortBy (flip compare `on` snd) (Map.toList topics)
            let rows = map (\(t, c) -> [show t, show c]) sorted
            
            printTable ["Topic", "Mentions"] rows

            -- 7. Parallel Analysis Demonstration
            putStrLn ""
            
            putStrLn "---- Total Words Exchanged ----"

            -- Below I am collecting all messages first
            allMsgs <- forM users $ \u -> readMVar (inbox u)
            -- Below I am calculating total words per user in parallel
            let wordsCount = parMap rpar (sum . map (length . words . content)) allMsgs
            
            let res = zipWith (\u c -> [username u, show c]) users wordsCount
            printTable ["User", "Total Words"] res
            putStrLn "(Calculated using Control.Parallel.Strategies)"

            putStrLn ""
            putStrLn "Simulation Finished."
            putStrLn "All the logs related to messages are store in the log file"

-- | Below I am printing a table with borders
printTable :: [String] -> [[String]] -> IO ()
printTable headers rows = do
    let allRows = headers : rows
        colWidths = foldr (zipWith max . map length) (replicate (length headers) 0) allRows
        
        -- Box drawing characters
        topLeft = "┌"
        topRight = "┐"
        botLeft = "└"
        botRight = "┘"
        horiz = "─"
        vert = "│"
        cross = "┼"
        topCross = "┬"
        botCross = "┴"
        
        -- Below I am handling ouput formatting
        mkLine l c r joins = l ++ intercalate joins (map (\w -> replicate (w + 6) (head horiz)) colWidths) ++ r
        
        topLine = mkLine topLeft topCross topRight topCross
        midLine = mkLine "├" cross "┤" cross
        botLine = mkLine botLeft botCross botRight botCross
        
        pad = "   "
        formatRow cols = vert ++ pad ++ intercalate (pad ++ vert ++ pad) (zipWith (\w s -> printf ("%-" ++ show w ++ "s") s) colWidths cols) ++ pad ++ vert
        
        headerRow = formatRow headers
        dataRows = map formatRow rows
        
    putStrLn topLine
    putStrLn headerRow
    putStrLn midLine
    mapM_ putStrLn dataRows
    putStrLn botLine

-- | Printing the messages
printBoxed :: String -> IO ()
printBoxed msg = do
    let linesList = lines msg
        width = maximum (map length linesList) + 4
        
        -- Unicode characters
        topLeft = "┌"
        topRight = "┐"
        botLeft = "└"
        botRight = "┘"
        horiz = "─"
        vert = "│"
    
    let mkLine l r = l ++ replicate width (head horiz) ++ r
        topLine = mkLine topLeft topRight
        botLine = mkLine botLeft botRight
        
        fmtLine s = vert ++ "  " ++ printf ("%-" ++ show (width - 4) ++ "s") s ++ "  " ++ vert
        
    putStrLn topLine
    mapM_ (putStrLn . fmtLine) linesList
    putStrLn botLine
