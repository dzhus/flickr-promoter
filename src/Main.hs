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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

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

import Servant.API
import Servant.Client

newtype PhotoId = PhotoId Text deriving newtype ToHttpApiData

newtype Tag = Tag Text

newtype Location = Location Text

data Photo = Photo
  { id       :: PhotoId
  , tags     :: Set Tag
  , location :: Location
  }

instance FromJSON Photo where
  parseJSON = withObject "Photo" $ \o ->
    o .: "location"

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
    Location loc = location photo

data FlickrMethod
  = GroupsGetInfo
  | PhotosGetInfo
  | PhotosRecentlyUpdated
  | TestLogin

instance ToHttpApiData FlickrMethod where
  toQueryParam GroupsGetInfo = "groups.get.info"
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

type FlickrAPI = QueryParam "method" FlickrMethod :> QueryParam "format" FlickrFormat :> Get '[JSON] LoginResponse :<|>
                 QueryParam "method" FlickrMethod :> QueryParam "format" FlickrFormat :> Get '[JSON] Photo

testLogin :<|> recentlyUpdated = client (Proxy :: Proxy FlickrAPI)

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

main :: IO ()
main = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr flickrApi
  (print =<<) $ runClientM (testLogin (Just TestLogin) (Just JsonFormat)) env
  putStrLn "hello world"
