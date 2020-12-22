{-# LANGUAGE OverloadedStrings#-}
module Main where

import Server
import Web.Scotty
import System.Environment
import DB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Data.Text as T

main :: IO ()
main = do
    [user, pass] <- map BC.pack <$> mapM getEnv ["POSTGRE_USER", "POSTGRE_PASS"]
    conn <- connDB (user, pass)
    putStrLn "connected!" 
    scotty 3000 $ do
        get "/" handleRoot
        get "/channels" handleChannelGet
        post "/channels" handleChannelPost
        get "/messages" handleMessageGet
        post "/messages" handleMessagePost
        get "/users" handleUsersGet
        post "/users" handleUsersPost

