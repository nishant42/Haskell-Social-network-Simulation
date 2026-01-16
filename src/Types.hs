module Types
    ( User(..)
    , Message(..)
    , Topic(..)
    ) where
-- i have used above code so that the Types module can only use User, Message, Topic for good coding practice

import Control.Concurrent.Chan (Chan)
import Control.Concurrent.MVar (MVar)

-- | below is the type for topic in social media simulation
data Topic 
    = Sports   -- ^ Discussion about sports
    | Tech     -- ^ Discussion about technology
    | Music    -- ^ Discussion about music
    | News     -- ^ General news
    | Random   -- ^ Random chatter
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | below is the type for message in social media simulation
data Message = Message
    { sender      :: String  -- ^ The username of the sender
    , content     :: String  -- ^ The text content of the message
    , topic       :: Topic   -- ^ The topic of the message ( The Extension feature)
    } deriving (Show)

-- | Below is the type for user in social media simulation
data User = User
    { username     :: String          -- ^ The user's unique name
    , inbox        :: MVar [Message]  -- ^ A thread safe queue for receiving messages (List used as stack/queue)
    , messageCount :: MVar Int        -- ^ A thread safe counter for received messages
    , sentCount    :: MVar Int        -- ^ A thread safe counter for sent messages
    , battery      :: MVar Int        -- ^ Battery level , which is 0 to 100
    }
