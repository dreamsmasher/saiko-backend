{-# LANGUAGE OverloadedStrings#-}
module Main where

import Lib
import Server
import Web.Scotty
import System.Environment
import Database.MongoDB

import MongoDB
import qualified Data.Text as T

main :: IO ()
main = do
    mongoUsername <- T.pack <$> getEnv "MONGO_USER"
    mongoPassword <- T.pack <$> getEnv "MONGO_PASS"
    putStrLn $ fmtConnUrl mongoUsername mongoPassword "SaikoCluster"
    scotty 3000 $ do
        get "/" handleRoot
        get "/channels" handleChannelGet
        post "/channels" handleChannelPost
        get "/messages" handleMessageGet
        post "/messages" handleMessagePost
        get "/users" handleUsersGet
        post "/users" handleUsersPost

