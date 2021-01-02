{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DB where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Int
import Data.Functor.Contravariant
import Data.Functor
import Data.Either
import Data.Maybe
import Contravariant.Extras
import Control.Monad
import Control.Arrow
import Control.Lens
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Hasql.Connection ( settings, Settings )
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as L
import Data.Time ( LocalTime, UTCTime, utc, utcToLocalTime, localTimeToUTC )
-- import PostgreSQL.Binary.Decoding 
import Opaleye as O
-- import Opaleye.Operators
import Data.Profunctor.Product
import Database.PostgreSQL.Simple 

import Types
-- saikoSettings :: B.ByteString
-- saikoSettings = "host=localhost port=5432 dbname=saiko connect_timeout=20"


type DBUsername = B.ByteString
type DBPassword = B.ByteString

type Username = Text
type Channelname = Text

saikoSettings :: DBUsername -> DBPassword -> ConnectInfo
saikoSettings u p = ConnectInfo "localhost" 5432 u' p' "saiko"
    where (u', p') = (C.unpack u, C.unpack p)

connDB :: DBUsername -> DBPassword -> IO Connection
connDB u p = connect $ saikoSettings u p

-- Ct = contravariant, Cv = covariant
-- Table is just a wrapper around Profunctor
type UserFieldCt = (Maybe (Field SqlInt4), Field SqlText, Field SqlText, FieldNullable SqlText, FieldNullable SqlInt4)
type UserField = (Field SqlInt4, Field SqlText, Field SqlText, FieldNullable SqlText, FieldNullable SqlInt4)

usersTable :: Table UserFieldCt UserField
usersTable = table "Users" $ p5 ( optionalTableField "id"
                                , tableField "username"
                                , tableField "password"
                                , tableField "saved_session"
                                , tableField "settings"
                                )
    
selectUsers :: Select UserField
selectUsers = selectTable usersTable

type SettingsFieldCt = (Maybe (Field SqlInt4), FieldNullable SqlInt4, FieldNullable SqlBool)
type SettingsField = (Field SqlInt4, FieldNullable SqlInt4, FieldNullable SqlBool)

settingsTable :: Table SettingsFieldCt SettingsField
settingsTable = table "Settings" $ p3 ( optionalTableField "id"
                                      , tableField "user_id"
                                      , tableField "darkmode"
                                      )

selectSettings :: Select SettingsField
selectSettings = selectTable settingsTable

type ChannelsFieldCt = (Maybe (Field SqlInt4), Field SqlText)
type ChannelsField = (Field SqlInt4, Field SqlText)

channelsTable :: Table ChannelsFieldCt ChannelsField
channelsTable = table "Channels" $ p2 ( optionalTableField "id"
                                      , tableField "channel_name"
                                      )

selectChannels :: Select ChannelsField
selectChannels = selectTable channelsTable

type ChnlUsrFieldCt = (Maybe (Field SqlInt4), FieldNullable SqlInt4, FieldNullable SqlInt4, FieldNullable SqlInt4)
type ChnlUsrField = (Field SqlInt4, FieldNullable SqlInt4, FieldNullable SqlInt4, FieldNullable SqlInt4)

channelsUsersTable :: Table ChnlUsrFieldCt ChnlUsrField 
channelsUsersTable = table "Channels_Users" $ p4 ( optionalTableField "id"
                                                 , tableField "user_id"
                                                 , tableField "channel_id"
                                                 , tableField "permissions"
                                                 )

selectChannelsUsers :: Select ChnlUsrField
selectChannelsUsers  = selectTable channelsUsersTable

type MessageFieldCt = (Maybe (Field SqlInt4), Field SqlText, Field SqlInt4, Field SqlInt4, Field SqlTimestamp)
type MessageField = (Field SqlInt4, Field SqlText, Field SqlInt4, Field SqlInt4, Field SqlTimestamp)

messagesTable :: Table MessageFieldCt MessageField
messagesTable = table "Messages" $ p5 ( optionalTableField "id"
                                      , tableField "body"
                                      , tableField "channel_id"
                                      , tableField "user_id"
                                      , tableField "msg_time"
                                      )

selectMessages :: Select MessageField
selectMessages = selectTable messagesTable

where_ :: Field PGBool -> Select ()
where_ = viaLateral restrict
-- for some reason it's not being exported??

getChnlName :: Field SqlInt4 -> Select (Field SqlText)
getChnlName channelId = do
    (ci, cn) <- selectChannels
    where_ $ ci .== channelId
    pure cn
    
getChnlId :: Field SqlText -> Select (Field SqlInt4)
getChnlId name = do
    (ci, cn) <- selectChannels
    where_ $ cn .== name
    pure ci

getUserName :: Field SqlInt4 -> Select (Field SqlText)
getUserName userId = do
    (i, u, _, _, _) <- selectUsers
    where_ $ i .== userId
    pure u

getUserId :: UsernameS -> Select (Field SqlInt4)
getUserId name = do
    (i, u, _, _, _) <- selectUsers
    where_ $ u .== name
    pure i

type UsernameS = Field SqlText
type ChannelS = Field SqlText
type UserId = Field SqlInt4
type ChannelId = Field SqlInt4

getMessages_ :: UsernameS -> Select (Field SqlInt4, Field SqlText, ChannelS, UsernameS, Field SqlTimestamp)
    -- Select (Field SqlInt4, Field SqlTimestamp, Field SqlText, UsernameS, ChannelS)
getMessages_ name = do
    user <- getUserId name
    (i, b, c, u, t) <- selectMessages
    where_ $ u .== user
    channel <- getChnlName c 
    pure (i, b, channel, name, t)


getMessages :: Connection -> Username -> IO [Message]
getMessages conn name = map mkMsg <$> runSelect conn (getMessages_ nameField)
    where nameField = sqlStrictText name -- :: UsernameS
          mkMsg (i, b, c, n, t) = Msg (Just i) b c n (localTimeToUTC utc t)

getPresChannels :: UserId -> Select (ChannelId, ChannelS)
getPresChannels uid = do
    (i, c) <- selectChannels
    where_ $ uid .== i
    pure (i, c)

-- getChannels :: Connection -> User -> IO (Maybe [Channel])
-- getChannels conn (User n i) = do
--     uid <-  maybe ((listToMaybe <$> runSelect conn . getUserId . sqlLazyText) n) (pure . Just) i
    -- pure undefined
  
getSession :: Connection -> Username -> IO [Maybe Text] 
getSession conn name = fmap (pack <$>) <$> runSelect conn getS
    where getS :: Select (FieldNullable SqlText)
          getS = do
              (_, u, _, s, _) <- selectUsers
              where_ $ u .== toFields name
              pure s

userIsAuth_ :: UsernameS -> ChannelS -> Select (Field SqlBool)
userIsAuth_ name chnl = do 
    uid <- toNullable <$> getUserId name -- no need to use getIds here
    cid <- toNullable <$> getChnlId chnl
    inSelect cid $ do
        (_, u, c, _) <- selectChannelsUsers
        where_ $ uid .== u
        pure c

userIsAuth :: Connection -> Username -> Channelname -> IO Bool
userIsAuth conn name  =  toFields
                     >>> userIsAuth_ (toFields name) 
                     >>> runSelect conn 
                     >>> (<&> fromMaybe False . listToMaybe) -- safer than head

fstPent :: (a, b, c, d, e) -> a
fstPent (a, _, _, _, _) = a -- to save from excessive pattern matching

noop :: Maybe OnConflict
noop = Just DoNothing

createUser_ :: Username -> Insert [Int]
createUser_ u = Insert usersTable values getId noop
    where values = [(Nothing, sqlStrictText u, sqlString "", O.null, O.null)]
          getId = rReturning fstPent

createUser :: Connection -> Username -> IO User
createUser conn name = do
    id <- listToMaybe <$> runInsert_ conn (createUser_ name)
    pure $ User (L.fromStrict name) id


getIds :: Connection -> Username -> Channelname -> IO (Maybe (UserId, ChannelId))
getIds conn u c = do
    let queries = zipWith ($) [getUserId, getChnlId] $ sqlStrictText <$> [u, c]
    ids <- traverse listToMaybe <$> mapM (runSelectI conn) queries
    pure $ ids <&> (map sqlInt4 >>> \[uid, cid] -> (uid, cid))
    -- yes, I'm aware that traverse and mapM are the same function

-- on confirmation of room admin/permiss, add user to room
-- user_id, channel_id
addToChannel_ :: UserId -> ChannelId -> Insert Int64
addToChannel_ u c = Insert channelsUsersTable values rCount noop
    where values = [(Nothing, toNullable u, toNullable c, O.null)]

addToChannel :: Connection -> Username -> Channelname -> IO (Maybe Int)
addToChannel conn u  =  getIds conn u 
                    >=> traverse (runInsert_ conn . uncurry addToChannel_) 
                    >=> fmap fromIntegral >>> pure 

createChannel_ :: ChannelS -> Insert [Int]
createChannel_ c = Insert channelsTable [(Nothing, c)] (rReturning fst) noop

createChannel :: Connection -> Channelname -> IO Channel
createChannel conn c  =  (runInsert_ conn . createChannel_ . sqlStrictText) c 
                     <&> Channel (L.fromStrict c) . listToMaybe

createMessage_ :: Field SqlText -> ChannelId -> UserId -> Field SqlTimestamp -> Insert [Int]
createMessage_ b c u t = Insert messagesTable values (rReturning fstPent) noop
    where values = [(Nothing, b, c, u, t)]

createMessage :: Connection -> Message -> IO (Maybe Message)
createMessage conn m@(Msg _ b c u t) = do -- ignore id since we'll get it when we insert
    let body = sqlLazyText b
        time = sqlLocalTime $ utcToLocalTime utc t
        [user, chnl] = map L.toStrict [u, c]
    ids <- getIds conn user chnl
    let mkMsg (uid, cid) = do
            msgId <- listToMaybe <$> runInsert_ conn (createMessage_ body cid uid time)
            pure $ m & mid .~ msgId 
    traverse mkMsg ids -- traverse :: (a -> IO b) -> (Maybe a) -> IO (Maybe b)
