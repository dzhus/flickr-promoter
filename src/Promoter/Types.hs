module Promoter.Types where

import ClassyPrelude hiding (any)
import Data.Aeson
import Servant.API hiding (uriQuery)

newtype PhotoId = PhotoId {unPhotoId :: Text}
  deriving newtype (FromJSON, IsString, Show, ToHttpApiData)

newtype GroupId = GroupId Text
  deriving newtype
    ( FromJSON,
      IsString,
      Ord,
      Eq,
      Show,
      ToHttpApiData
    )

newtype UserId = UserId Text
  deriving newtype
    ( FromJSON,
      IsString,
      Ord,
      Eq,
      Show,
      ToHttpApiData
    )

newtype Tag = Tag Text
  deriving (Eq, Ord, Show)
  deriving newtype (IsString)

newtype Location = Location {unLocation :: Text}
  deriving (Show)

data Photo = Photo
  { id :: PhotoId,
    title :: Text,
    tags :: Set Tag,
    groups :: Set GroupId,
    location :: Maybe Location,
    views :: Word,
    faves :: Word
  }
  deriving (Show)
