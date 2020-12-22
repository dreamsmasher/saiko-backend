{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Server where

import Data.Aeson
import Data.ByteString.Lazy
import Network.Wai.Handler.Warp
import Control.Monad
import Control.Arrow
import Web.Scotty
import Network.HTTP.Types
import Control.Lens
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)

import Types

parseThenDo' :: (FromJSON a) => (ByteString -> Maybe a) -> ActionM () -> (a -> ActionM ()) -> ActionM ()
parseThenDo' d n j = do
    b <- body
    z <- (pure . d) b
    case z of
        Nothing -> status status400 >> n
        Just a -> status status200 >> j a

-- little hack to avoid polymorphism nonsense
parseThenDo :: (FromJSON a) => ActionM () -> (a -> ActionM ()) -> ActionM ()
parseThenDo = parseThenDo' decode

handleRoot :: ActionM ()
handleRoot = text "saiko is running"

handleChannelPost :: ActionM ()
handleChannelPost = parseThenDo
    (text "you dun goofed")
    $ text . ("room accepted: " <>) . view channelName
    -- b <- body
    -- c <- (pure . decode) b :: ActionM (Maybe Channel)
    -- case c of
    --     Nothing -> status status400 >> text "you dun goofed"
    --     Just channel -> do
    --         status status200
    --         text $ "room accepted: " <> (view channelname channel) -- add room to db

handleChannelGet :: ActionM ()
handleChannelGet = text "channels"

handleMessageGet :: ActionM ()
handleMessageGet = status status200 >> (text . decodeUtf8 . encode) (Msg "normie" "lobby" "this is a message" 10)

handleMessagePost :: ActionM ()
handleMessagePost = parseThenDo
    (text "invalid message")
    (text . T.pack . show . (id :: Message -> Message)) -- temp hack to get it to type check

handleUsersGet :: ActionM ()
handleUsersGet = text "users"

handleUsersPost :: ActionM ()
handleUsersPost = parseThenDo
    (text "invalid user")
    (text. T.pack . show . (id :: User -> User))