{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE LambdaCase#-}
{-# LANGUAGE OverloadedStrings#-}

module Types where

import Data.Aeson
import Data.Aeson.TH
import Control.Lens.TH
import Data.Text.Lazy hiding (drop)

data Message = Msg { _Musername :: Text
                   , _Mchannel :: Int
                   , _Mtext :: Text
                   , _Mtime :: Int
                   } deriving (Eq, Show)

deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''Message
makeLenses ''Message


data Channel = Channel { _channelname :: Text} deriving (Show)

deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''Channel
makeLenses ''Channel