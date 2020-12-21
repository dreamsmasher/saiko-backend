{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module MongoDB where

import Database.MongoDB
import Control.Monad.Trans (liftIO)
import Control.Monad
import Control.Arrow
import Text.Printf (printf)
import Data.Text


type DBName = Text
fmtConnUrl :: Username -> Password -> DBName -> String
fmtConnUrl = printf "mongodb+srv://%s:%s@saikocluster.bgwfn.mongodb.net/%s?retryWrites=true&w=majority"