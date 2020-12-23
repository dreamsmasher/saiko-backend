{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Server where

import Data.Aeson
import Data.ByteString.Lazy
-- import Network.Wai.Handler.Warp
import Control.Monad
import Control.Arrow
import Web.Scotty
import Control.Monad.Trans
import Network.HTTP.Types
import Control.Lens
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Hasql.Connection (Connection)
import Hasql.Session
import Hasql.Statement (Statement)
import Data.Time

import DB

import Types

type SaikoM a = Connection -> ActionM a

sts200, sts400, sts500 :: ActionM ()
sts200 = status status200
sts400 = status status400
sts500 = status status500

runSaiko :: Statement a b -> a -> SaikoM (Either QueryError b)
runSaiko f a conn = do
    let session = statement a f
    liftIO (run session conn)

parse :: (FromJSON a) => ActionM (Maybe a)
parse = decode <$> body

dbCheck :: Either QueryError a -> ActionM ()
dbCheck = either handleErr (const (text "success" >> sts200))
    where handleErr err = do
            sts400
            text (T.pack $ show err)

handleRoot :: ActionM ()
handleRoot = text "saiko is running"

handleChannelPost :: SaikoM ()
handleChannelPost conn = do
    c <- parse
    case c of
        Nothing -> sts400 >> text "you dun goofed"
        Just channel -> do
            res <- (flip (runSaiko createChannel) conn . T.toStrict . view channelName) channel
            dbCheck res

handleChannelGet :: ActionM ()
handleChannelGet = text "channels"

handleMessageGet :: ActionM ()
handleMessageGet = liftIO getCurrentTime >>= \u -> sts200 >> (text . decodeUtf8 . encode) (Msg u "ping, we got your msg" "normie" "lobby")

handleMessagePost :: SaikoM ()
handleMessagePost conn = do
    m <- parse
    case m of
        Nothing -> sts400 >> text "invalid message!!!"
        Just msg -> do
            let (Msg t b u c) = msg
            let [b', u', c'] = T.toStrict <$> [b, u, c]
            res <- runSaiko createMessage (Just t, b', u', c') conn
            dbCheck res

handleUsersGet :: ActionM ()
handleUsersGet = text "users"

handleUsersPost :: SaikoM ()
handleUsersPost conn = do
    u <- parse
    case u of
        Nothing -> sts400 >> text "invalid user!!!!"
        Just user -> do
            let name = (T.toStrict . view username) user
            res <- runSaiko createUser name conn
            dbCheck res
