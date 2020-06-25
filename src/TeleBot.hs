{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module TeleBot where

import           Network.HTTP.Conduit
import           Network.HTTP.Types         (queryTextToQuery)
import           Network.HTTP.Simple        (httpLBS, httpJSON, getResponseBody)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           Data.Maybe                 (fromJust, isJust)
import           Data.Aeson.Types


data Bot = Bot
    { token :: String
    , apiUrl :: String
    , onMessage :: Message -> IO (Maybe String)
    }

newBot = Bot
    { token = undefined
    , apiUrl = "https://api.telegram.org/bot"
    , onMessage = defaultOnMessage
    }


defaultOnMessage :: Message -> IO (Maybe String)
defaultOnMessage msg = do
    print $ messageText msg
    pure Nothing


data Message = Message
    { updateId :: Int
    , messageText :: String
    , chatId :: Int
    } deriving (Show, Generic)

instance FromJSON Message where
    parseJSON = withObject "Message" $ \obj -> do
        message <- obj .: "message"
        chat <- message .: "chat"
        updateId <- obj .: "update_id"
        messageText <- message .:? "text" .!= ""
        chatId <- chat .: "id"
        return (Message updateId messageText chatId)


data ApiMethod = GetUpdates | SendMessage deriving (Enum, Show)
type RequestParams = [(String, String)]

buildRequest :: Bot -> ApiMethod -> RequestParams -> Request
buildRequest bot method params = case parseUrlThrow url of
    Just req -> setQueryString (queryTextToQuery params') req
    Nothing -> error $ "Wrong URL: " ++ url
    where
        params' = map (\(key, val) -> (T.pack key, Just $ T.pack val)) params
        url = concat [apiUrl bot, token bot, "/", show method]


getMessages :: Bot -> Int -> IO [Message]
getMessages bot offset = do
    resp <- httpJSON $ buildRequest bot GetUpdates [("offset", show offset), ("timeout", "10")]
    let respBody = getResponseBody resp :: Object
    let updates = fromJust $ parseMaybe ( .: "result") respBody :: [Message]
    return $ filter (not . null . messageText) updates


sendMessage :: Bot -> Int -> String -> IO ()
sendMessage bot chatId msgText = do
    resp <- httpLBS $ buildRequest bot SendMessage [("chat_id", show chatId), ("text", msgText)]
    return ()


startBot :: Bot -> IO ()
startBot bot = loop 1
    where
        loop offset = do
            messages <- getMessages bot offset
            answers <- mapM (onMessage bot) messages
            mapM_ (uncurry $ sendMessage bot)
                [(chatId msg, fromJust ans) | msg <- messages, ans <- answers, isJust ans]

            let maxUpdateId = case messages of
                    [] -> offset
                    _ -> maximum $ map updateId messages
            loop $ maxUpdateId + 1
    
