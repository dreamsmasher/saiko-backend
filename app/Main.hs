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
    [mongoUser, mongoPass] <- (map T.pack) <$> mapM getEnv ["MONGO_USER", "MONGO_PASS"]
    -- let uri = fmtConnUrl mongoUser mongoPass "SaikoCluster"
    -- putStrLn uri
    connectAtlas (MAuth mongoUser mongoPass)
    scotty 3000 $ do
        get "/" handleRoot
        get "/channels" handleChannelGet
        post "/channels" handleChannelPost
        get "/messages" handleMessageGet
        post "/messages" handleMessagePost
        get "/users" handleUsersGet
        post "/users" handleUsersPost

