{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DB ( connDB
          , createUser
          , createChannel
          , addToChannel
          , createMessage
          ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Functor.Contravariant
import Data.Either
import Contravariant.Extras
import Control.Monad
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Hasql.Connection
import Hasql.Statement ( Statement(..) )
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import Data.Time ( LocalTime, UTCTime, utc, utcToLocalTime, localTimeToUTC )
import PostgreSQL.Binary.Decoding 

import Types
-- saikoSettings :: B.ByteString
-- saikoSettings = "host=localhost port=5432 dbname=saiko connect_timeout=20"

type DBUsername = B.ByteString
type DBPassword = B.ByteString

type Username = Text
type Channelname = Text

saikoSettings :: DBUsername -> DBPassword -> Settings
saikoSettings u p = settings "localhost" 5432 u p "saiko"

textParam :: E.Params Text
textParam = E.param $ E.nonNullable E.text

textDecode :: D.Row Text
textDecode = D.column $ D.nonNullable D.text

lazyText :: D.Row L.Text
lazyText = (D.column . D.nonNullable . (L.fromStrict <$>)) D.text

connDB :: (DBUsername, DBPassword) -> IO Connection
connDB = (acquire . uncurry saikoSettings) >=> either (print >=> const (exitWith $ ExitFailure 1)) pure

createUser :: Statement Username ()
createUser = Statement sql textParam D.noResult True
    where sql = "insert into Users (username) values ($1)"

-- on confirmation of room admin/permiss, add user to room
-- user_id, channel_id
addToChannel :: Statement (Username, Channelname) ()
addToChannel = Statement sql enc D.noResult True
    where sql = C.unwords [ "insert into Channels_Users (user_id, channel_id)"
                          , "select U.id, C.id from Users as U, Channels as C"
                          , "where U.username=$1 and C.channel_name=$2"
                          ]
          enc = join contrazip2 textParam

-- getUser :: Statement Text  
getUser :: Statement Text (Maybe Text)
getUser = Statement sql enc dec True
    where sql = "select saved_session from users where username=$1"
          enc = textParam
          dec = D.singleRow (D.column $ D.nullable D.text) 

getMessages :: Statement Channelname [Message]
getMessages = Statement sql enc dec True
    where sql = C.unwords [ "select M.msg_time, M.body, U.name, $1 as channel from Messages as M"
                          , "join Users as U on U.id=M.user_id"
                          , "where M.channel_id in"
                          , "(select id from channels where channel_name=$1)"
                          ]
          enc = textParam
          dec = D.rowList $ Msg . localTimeToUTC utc
            <$> (D.column . D.nonNullable) D.timestamp  
            <*> lazyText
            <*> lazyText
            <*> lazyText

getChannel :: Statement Channelname Channel 
getChannel = Statement "select channel_name, id from channels where channel_name=$1" enc dec True
    where enc = textParam
          dec = D.singleRow $ Channel 
            <$> lazyText 
            <*> (D.column . D.nullable . (fromIntegral <$>)) D.int4

-- 
createMessage :: Statement (Maybe UTCTime, Text, Username, Channelname) ()
createMessage = Statement sql enc D.noResult True
    where sql = C.unwords [ "insert into messages (msg_time, body, user_id, channel_id)"
                          , "values ($1, $2,"
                          , "(select id from Users where username=$3)"
                          , ",(select id from Channels where channel_name=$4))"
                          ]
          fromUtc = E.param $ E.nullable (E.timestamp >$$< utcToLocalTime utc)
          enc = contrazip4 fromUtc textParam textParam textParam

createChannel :: Statement Text ()
createChannel = Statement sql textParam D.noResult True
    where sql = "insert into channels (channel_name) values ($1)"