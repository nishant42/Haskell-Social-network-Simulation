module User
    ( simUser
    ) where
-- i have used above code so that the User module can only use simUser for good coding practice
import Types
import Interfaces
import Control.Monad (when, void)
import Text.Printf (printf)


simUser :: SocialLife m => User -> [User] -> m ()
simUser me allUsers = do
    loop
  where
    loop = do
        -- 1. Here we are generating random delay
        delayT <- randInt (10000, 100000)
        sleep delayT
        
        -- 2. Here we are Checking Battery
        alive <- isBatteryAlive me
        if not alive 
            then logStr $ "--- " ++ username me ++ " run out of battery ---"
            else do
                -- 3. Here we are checking Limit
                -- Returns (DidIncrement/Success, IsLimitReached)
                (canSend, limitReached) <- checkLimit
                
                if canSend 
                    then do
                         -- 4. Logic: Send Message
                         performAction
                         
                         if limitReached 
                             then signalEnd
                             else loop
                    else return () -- Limit already reached, stop thread

    performAction = do
        -- Drain Battery
        newBatt <- drainBattery me 2
        
        -- Picking the  Recipient
        let others = filter (\u -> username u /= username me) allUsers
        idx <- randInt (0, length others - 1)
        let recipient = others !! idx
        
        -- Picking the Topic
        let topics = [minBound .. maxBound] :: [Topic]
        tIdx <- randInt (0, length topics - 1)
        let topic = topics !! tIdx
        
        let txt = "Hi, today's topic to discuss is " ++ show topic
        let msg = Message { sender = username me, content = txt, topic = topic }
        
        -- Here we are sending the message
        sendMsg recipient msg
        
        -- Here we are recording the stats
        recordStats topic me recipient
        
        -- Here we are logging the message
        now <- getNow
        logStr $ printf "[%s] %s (Batt: %d%%) -> %s (Topic: %s)" now (username me) newBatt (username recipient) (show topic)
