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
import Data.Time ( UTCTime, utc, utcToLocalTime )
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