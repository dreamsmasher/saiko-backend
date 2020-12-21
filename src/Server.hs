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

import Types

handleChannelPost :: ActionM ()
handleChannelPost = do
    b <- body
    c <- (pure . decode) b :: ActionM (Maybe Channel)
    case c of
        Nothing -> status status400 >> text "you dun goofed"
        Just channel -> do
            status status200
            text $ "room accepted: " <> (view channelname channel) -- add room to db


handleChannelGet :: ActionM ()
handleChannelGet = undefined

handleMessageGet :: ActionM ()
handleMessageGet = status status200 >> text "messages"

handleMessagePost :: ActionM ()
handleMessagePost = do
    b <- body
    m <- (pure . decode) b :: ActionM (Maybe Message)
    case m of
        Nothing -> status status400 >> text "invalid message"
        Just msg -> do
            status status200
            text . T.pack . show $ msg
