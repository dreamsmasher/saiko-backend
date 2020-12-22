{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module MongoDB where

import Database.MongoDB
import Control.Monad.Trans (liftIO)
import Control.Monad
import Control.Arrow
import Text.Printf (printf)
import Data.Text hiding (map)
import qualified Database.MongoDB.Transport.Tls as DBTLS


type DBName = Text
data MongoAuth = MAuth { mongoUser :: Username
                       , mongoPass :: Password
                       } deriving (Eq, Show, Read)
repSetName = "mongodb+srv://saikocluster.bgwfn.mongodb.net/"
fmtConnUrl :: Username -> Password -> DBName -> String
fmtConnUrl = printf "mongodb://%s:%s@saikocluster-shard-00-00.bgwfn.mongodb.net:27017,saikocluster-shard-00-01.bgwfn.mongodb.net:27017,saikocluster-shard-00-02.bgwfn.mongodb.net:27017/%s?ssl=true&replicaSet=atlas-a0dnjx-shard-0&authSource=admin&retryWrites=true&w=majority"
    -- printf "mongodb+srv://%s:%s@saikocluster.bgwfn.mongodb.net/%s?retryWrites=true&w=majority"

connectAtlas :: MongoAuth -> IO Pipe
connectAtlas m@(MAuth user pass)= do
  putStrLn "connecting to rep"
  let uri = fmtConnUrl user pass "admin"
  putStrLn uri
  pipe' <- connect . host $ fmtConnUrl user pass "admin"
  p <- access pipe' master "admin" $ auth user pass
  putStrLn "it do be workin"
  -- isAuth <- access pipe master "SaikoCluster" (auth user pass)
  -- if (isAuth) then
    -- putStrLn "connected"
  -- else 
    -- putStrLn "failed"
  pure pipe'