module Main where

import System.Environment
import TeleBot


handleMessage :: Message -> IO (Maybe String)
handleMessage msg = do
    print msg
    return $ Just $ messageText msg

main = do
    botToken <- getEnv "BOT_TOKEN"
    let myBot = newBot
            { token = botToken
            , onMessage = handleMessage
            }
    startBot myBot
