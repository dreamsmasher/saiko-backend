{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DB where
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Functor.Contravariant
import Control.Monad
import System.Exit
import Hasql.Connection
import Hasql.Statement
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Data.Text (Text)
import qualified Data.Text as T
-- saikoSettings :: B.ByteString
-- saikoSettings = "host=localhost port=5432 dbname=saiko connect_timeout=20"

type PUsername = B.ByteString
type PPassword = B.ByteString

type Username = Text
type Channelname = Text

saikoSettings :: PUsername -> PPassword -> Settings
saikoSettings u p = settings "localhost" 5432 u p "saiko"

connDB :: (PUsername, PPassword) -> IO Connection
connDB auth = do
    connAttempt <- (acquire . uncurry saikoSettings) auth
    case connAttempt of
        Left err -> print err >> exitWith (ExitFailure 1)
        Right conn -> pure conn

createUser :: Statement Username ()
createUser = Statement sql enc D.noResult True
    where sql = "insert into Users (username) values ($1)"
          enc = E.param $ E.nonNullable E.text

-- select id from Channels where channel_name='test'

-- insert into channels_users (user_id, channel_id) values(1, 2)


-- on confirmation of room admin/permiss, add user to room
-- user_id, channel_id
addToChannel :: Statement (Username, Channelname) ()
addToChannel = Statement sql enc D.noResult True
    where sql = C.unwords $ [ "insert into Channels_Users (user_id, channel_id)"
                            , "select U.id, C.id from Users as U, Channels as C"
                            , "where U.username=$1 and C.channel_name=$2"
                            ]
          enc = (fst >$< E.param (E.nonNullable E.text)) <>
                (snd >$< E.param (E.nonNullable E.text))



addToMessages :: Statement (Text, Text, Username, Channelname) ()
addToMessages = Statement sql enc D.noResult True
    where sql = C.unwords $ ["insert into messages (body, msg_time, user_id, channel_id)"
                            , "values ('$1', '$2', "
                            , "(select id from Users where username=$3)"
                            , "(select id from Channels where channel_name=$4))"
                            ]
