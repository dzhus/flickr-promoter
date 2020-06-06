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

import Servant.API
import Servant.Client

newtype PhotoId = PhotoId Text

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

type FlickrAPI = QueryParam "method" :> QueryParam "photo_id" PhotoId :> Get '[JSON] Photo

recentlyUpdated = client (Proxy :: Proxy FlickrAPI)

rules = [ any .=> "34427469792@N01"
        , locatedIn "Prague" .=> "48889111127@N01"
        , locatedIn "Bavaria" .=> "860590@N23"
        , locatedIn "Crimea" .=> "60453939@N00"
        , locatedIn "Lyon" .=> "13409106@N00"
        ]

flickrApi = "https://www.flickr.com/services/rest/"

main :: IO ()
main = do
  putStrLn "hello world"
