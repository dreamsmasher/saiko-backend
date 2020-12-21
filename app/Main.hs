{-# LANGUAGE OverloadedStrings#-}
module Main where

import Lib
import Server
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        text "saiko is running"
    get "/channels" $ do
        text "henlo"
    post "/channels" handleChannelPost
    get "/messages" handleMessageGet
    post "/messages" handleMessagePost

