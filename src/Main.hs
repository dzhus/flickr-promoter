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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import ClassyPrelude hiding (any)

import GHC.Records
import Data.Aeson
import Data.Proxy
import GHC.TypeLits

import Network.HTTP.Client.TLS
import Network.HTTP.Media ((//), (/:))

import Servant.API
import Servant.Client
import Servant.Client.Core

newtype PhotoId = PhotoId Text
  deriving newtype (IsString, Show, ToHttpApiData)

newtype Tag = Tag Text
  deriving Show

newtype Location = Location { unLocation :: Text }
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

any :: Photo -> Bool
any = const True

-- | >>> locatedIn "UK"
-- False
locatedIn :: Text -> Photo -> Bool
locatedIn text = isInfixOf text . unLocation . getField @"location"

data FlickrMethod (a :: Symbol)

instance (KnownSymbol method, HasClient m api) => HasClient m (FlickrMethod method :> api) where
  type Client m (FlickrMethod method :> api) = Client m api

  clientWithRoute pm _ req = clientWithRoute pm (Proxy :: Proxy api) req'
    where
      req' = appendToQueryString "method" (Just (fromString $ symbolVal (Proxy :: Proxy method))) req

  hoistClientMonad pm _ f cl =
    hoistClientMonad pm (Proxy :: Proxy api) f cl

data FlickrResponseFormat = JsonFormat

instance HasClient m api => HasClient m (FlickrResponseFormat :> api) where
  type Client m (FlickrResponseFormat :> api) = Client m api

  clientWithRoute pm _ = clientWithRoute pm (Proxy :: Proxy api) .
    appendToQueryString "nojsoncallback" (Just "1") .
    appendToQueryString "format" (Just "json")

  hoistClientMonad pm _ f cl =
    hoistClientMonad pm (Proxy :: Proxy api) f cl

data FlickrContent = FlickrContent
  { _content :: Text }
  deriving (Generic, FromJSON, Show)

data FlickrUser = FlickrUser
  { username :: FlickrContent }
  deriving (Generic, FromJSON, Show)

data LoginResponse = LoginResponse
  { user :: FlickrUser }
  deriving (Generic, FromJSON, Show)

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

type FlickrAPI = FlickrResponseFormat :> (QueryParam "api_key" Text :> FlickrMethod "flickr.test.login" :> Get '[JSON] LoginResponse :<|>
                                          QueryParam "api_key" Text :> FlickrMethod "flickr.photos.getInfo" :> QueryParam "photo_id" PhotoId :> Get '[JSON] PhotoResponse)

testLogin :<|> photosGetInfo = client (Proxy :: Proxy FlickrAPI)

-- TODO Wrap raw methods in something that will automatically provide
-- api_key, method and format

rules :: [Rule]
rules = [ any .=> "34427469792@N01"
        , locatedIn "Prague" .=> "48889111127@N01"
        , locatedIn "Bavaria" .=> "860590@N23"
        , locatedIn "Crimea" .=> "60453939@N00"
        , locatedIn "Lyon" .=> "13409106@N00"
        ]

flickrApi :: BaseUrl
flickrApi = BaseUrl Https "www.flickr.com" 443 "/services/rest/"

postToGroup = error "postToGroup"

fetchMyPhotos :: m [Photo]
fetchMyPhotos = error "fetchMyPhotos"

process :: IO ()
process = do
  photos <- fetchMyPhotos
  forM_ photos $ \p -> do
    let photoGroups = mapMaybe (\(Rule (test, g)) -> if test p then Just g else Nothing) rules
    forM_ photoGroups (postToGroup photo)

apiKey :: Maybe Text
apiKey = Just "bad7960ebd9a9742de19b51b84f70d4a"

main :: IO ()
main = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr flickrApi
  (print =<<) $ runClientM (testLogin apiKey) env
  (print =<<) $ runClientM (photosGetInfo apiKey (Just "48819805098")) env
