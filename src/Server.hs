{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Server where

-- import Network.Wai.Handler.Warp

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import DB
import Data.Aeson
import Data.Aeson.Parser
import Data.ByteString.Lazy as L
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time
import Database.PostgreSQL.Simple
import Network.HTTP.Types
import Types
import Web.Scotty
import qualified Data.HashMap.Strict as M

type SaikoM a = Connection -> ActionM a

sts200, sts400, sts500 :: ActionM ()
sts200 = status status200
sts400 = status status400
sts500 = status status500

quickParse :: ByteString -> Maybe Object
quickParse = 
  decodeStrictWith json' (\case {Object a -> Success a; _ -> Error "parse"}) . L.toStrict 

parse :: (FromJSON a) => ActionM (Maybe a)
parse = decode <$> body

-- dbCheck :: Either QueryError a -> ActionM ()
-- dbCheck = either handleErr (const (sts200 >> text "success"))
--   where
--     handleErr err = do
--       sts400
--       text (T.pack $ show err)

handleRoot :: ActionM ()
handleRoot = text "saiko is running"

handleChannelPost :: SaikoM ()
handleChannelPost conn = do
  c <- parse
  case c of
    Nothing -> sts400 >> text "you dun goofed"
    Just channel -> do
      res <- liftIO $ (createChannel conn . T.toStrict . view channelName) channel
      -- res <- (flip (runSaiko createChannel) conn . T.toStrict . view channelName) channel
      sts200

handleChannelGet :: ActionM ()
handleChannelGet = text "channels get successful"

handleMessageGet :: SaikoM ()
handleMessageGet conn = do
  obj <- (decode :: ByteString -> Maybe UserChnl) <$> body
  case obj of 
    Nothing -> sts400 >> text "invalid input, couldn't parse"
    Just (USC user chnl) -> do
      auth <- runSaiko userIsAuth (join (***) T.toStrict (user, chnl)) conn
      dbCheck auth
      pure ()

handleMessagePost :: SaikoM ()
handleMessagePost conn = do
  m <- parse
  case m of
    Nothing -> sts400 >> text "invalid message!!!"
    Just msg -> do
      let (Msg i b c u t) = msg
      let [b', u', c'] = T.toStrict <$> [b, u, c]
      res <- runSaiko createMessage (Just t, b', u', c') conn
      dbCheck res

handleUsersGet :: SaikoM ()
handleUsersGet conn = do
  obj <- quickParse <$> body
  case obj of 
    Just r -> sts200 >> liftIO (print . M.lookup "username" $ r) >> text "parsed"
    _ -> sts400 >> text "invalid"

handleUsersPost :: SaikoM ()
handleUsersPost conn = do
  u <- parse
  case u of
    Nothing -> sts400 >> text "invalid user!!!!"
    Just user -> do
      let name = (T.toStrict . view username) user
      res <- runSaiko createUser name conn
      dbCheck res
