-- | So the plan is:
--
-- Fetch list of photos:
--
-- https://www.flickr.com/services/api/flickr.photos.recentlyUpdated.html
--
-- Fetch information for each photo (with caching!)
--
-- https://www.flickr.com/services/api/explore/flickr.photos.getInfo
--
-- Find what to post where
--
-- (Optional) Check group posting limits
--
-- https://www.flickr.com/services/api/flickr.groups.getInfo.html
--
-- Post until there're no groups to add photos to
--
-- https://www.flickr.com/services/api/flickr.groups.pools.add.html

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import ClassyPrelude hiding (any)
import qualified ClassyPrelude as P

import GHC.Records
import Data.Aeson
import Data.Proxy
import Data.Set
import Data.String
import GHC.Generics

import Network.HTTP.Client.TLS
import Network.HTTP.Media ((//), (/:))

import Servant.API
import Servant.Client

newtype PhotoId = PhotoId Text
  deriving newtype (IsString, Show, ToHttpApiData)

newtype Tag = Tag Text
  deriving Show

newtype Location = Location Text
  deriving Show

data Photo = Photo
  { id       :: PhotoId
  , tags     :: Set Tag
  , location :: Location
  }
  deriving Show

instance FromJSON Photo where
  parseJSON = withObject "Photo" $ \o ->
    o .: "photo"

newtype GroupId = GroupId Text deriving newtype IsString

newtype Rule = Rule (Photo -> Bool, GroupId)

(.=>) :: (Photo -> Bool) -> GroupId -> Rule
(.=>) isSource target = Rule (isSource, target)

any = const True

-- | >>> locatedIn "UK"
-- False
locatedIn :: Text -> Photo -> Bool
locatedIn text photo = text `isInfixOf` loc
  where
    Location loc = getField @"location" photo

data FlickrMethod
  = GroupsGetInfo
  | PhotosGetInfo
  | PhotosRecentlyUpdated
  | TestLogin

instance ToHttpApiData FlickrMethod where
  toQueryParam GroupsGetInfo = "flickr.groups.getInfo"
  toQueryParam PhotosGetInfo = "flickr.photos.getInfo"
  toQueryParam PhotosRecentlyUpdated = "flickr.photos.recentlyUpdated"
  toQueryParam TestLogin = "flickr.test.login"

data FlickrFormat = JsonFormat

instance ToHttpApiData FlickrFormat where
  toQueryParam _ = "json"

data FlickrContent = FlickrContent
  { _content :: Text }
  deriving (Generic, FromJSON, Show)

data FlickrUser = FlickrUser
  { username :: FlickrContent }
  deriving (Generic, FromJSON, Show)

data LoginResponse = LoginResponse
  { user :: FlickrUser }
  deriving (Generic, FromJSON, Show)

data FlickrJSON

data PhotoResponse = PhotoResponse
  { photo :: FlickrPhoto }
  deriving (Generic, FromJSON, Show)

data FlickrLocation = FlickrLocation
  { country :: FlickrContent
  , region :: FlickrContent
  , county :: FlickrContent
  , locality :: FlickrContent
  }
  deriving (Generic, FromJSON, Show)

data FlickrPhoto = FlickrPhoto
  { location :: FlickrLocation
  , tags     :: FlickrTags
  }
  deriving (Generic, FromJSON, Show)

data FlickrTags = FlickrTags
  { tag :: [FlickrContent] }
  deriving (Generic, FromJSON, Show)

instance Accept FlickrJSON where
  contentType _ = "text" // "javascript" /: ("charset", "utf-8")

instance FromJSON a => MimeUnrender FlickrJSON a where
  mimeUnrender _ = eitherDecode . dropPrefix "jsonFlickrApi(" . dropSuffix ")"

type FlickrAPI = QueryParam "api_key" Text :> QueryParam "method" FlickrMethod :> QueryParam "format" FlickrFormat :> Get '[FlickrJSON] LoginResponse :<|>
                 QueryParam "api_key" Text :> QueryParam "method" FlickrMethod :> QueryParam "format" FlickrFormat :> QueryParam "photo_id" PhotoId :> Get '[FlickrJSON] PhotoResponse

testLogin :<|> photosGetInfo = client (Proxy :: Proxy FlickrAPI)

-- TODO Wrap raw methods in something that will automatically provide
-- api_key, method and format

rules = [ any .=> "34427469792@N01"
        , locatedIn "Prague" .=> "48889111127@N01"
        , locatedIn "Bavaria" .=> "860590@N23"
        , locatedIn "Crimea" .=> "60453939@N00"
        , locatedIn "Lyon" .=> "13409106@N00"
        ]

flickrApi = BaseUrl Https "www.flickr.com" 443 "/services/rest/"

postToGroup = error "postToGroup"

fetchMyPhotos :: m [Photo]
fetchMyPhotos = error "fetchMyPhotos"

process :: IO ()
process = do
  photos <- fetchMyPhotos
  forM_ photos $ \photo -> do
    let photoGroups = mapMaybe (\(Rule (pred, g)) -> if pred photo then Just g else Nothing) rules
    forM_ photoGroups (postToGroup photo)

apiKey :: Maybe Text
apiKey = Just "bad7960ebd9a9742de19b51b84f70d4a"

main :: IO ()
main = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr flickrApi
  (print =<<) $ runClientM (testLogin apiKey (Just TestLogin) (Just JsonFormat)) env
  (print =<<) $ runClientM (photosGetInfo apiKey (Just PhotosGetInfo) (Just JsonFormat) (Just "48819805098")) env
