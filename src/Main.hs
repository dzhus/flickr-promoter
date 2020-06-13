-- | So the plan is:
--
-- Fetch list of photos:
--
-- https://www.flickr.com/services/api/flickr.photos.recentlyUpdated.html
--
-- Fetch information for each photo, meta and which pools is it a
-- member of (with caching!)
--
-- https://www.flickr.com/services/api/explore/flickr.photos.getInfo
--
-- https://www.flickr.com/services/api/flickr.photos.getAllContexts.html
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

import Data.Aeson
import Data.Proxy
import GHC.Records
import GHC.TypeLits
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Servant.Client.Core
import Turtle.Format (format, (%), s)
import Web.Authenticate.OAuth
import qualified Text.URI as URI

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

-- TODO Select only public photos

testLogin :<|> photosGetInfo = client (Proxy :: Proxy FlickrAPI)

-- TODO Wrap raw methods in something that will automatically provide
-- api_key

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

-- TODO Is this superseded by
-- &oauth_consumer_key=653e7a6ecc1d528c516cc8f92cf98611 in API
-- requests? https://www.flickr.com/services/api/auth.oauth.html
apiKey :: Text
apiKey = "53eeb65b3ecfc822e4cdfa8440e058fd"

apiSecret = "2f5d176193666a48"

flickrOAuth :: OAuth
flickrOAuth = newOAuth{ oauthServerName = "Flickr"

                      -- URLs from https://www.flickr.com/services/api/auth.oauth.html
                      , oauthRequestUri = "https://www.flickr.com/services/oauth/request_token"
                      , oauthAuthorizeUri = "https://www.flickr.com/services/oauth/authorize"
                      , oauthAccessTokenUri = "https://www.flickr.com/services/oauth/access_token"

                      , oauthConsumerKey = encodeUtf8 apiKey
                      -- Secret from https://www.flickr.com/services/apps/by/...
                      , oauthConsumerSecret = apiSecret
                      , oauthCallback = Just "https://gist.github.com/dzhus/0bf2a8b1990c288315411ce69bca56df"
                      }


-- Results from authorize URL redirect
persistedAccessToken :: Maybe Credential
persistedAccessToken = Just Credential {unCredential = [("fullname","Dmitry Djouce"),("oauth_token","72157714614929972-9e8de28f8f2bf657"),("oauth_token_secret","4366f00aefaa68dd"),("user_nsid","46721940@N00"),("username","Dmitry Djouce")]}

-- Perform OAuth 1.0a authorisation with Flickr
auth :: Manager -> IO Credential
auth mgr = do
  tmpCred <- getTemporaryCredential flickrOAuth mgr

  let authorizationUrl = format (s % "&perms=write") $ fromString $ authorizeUrl flickrOAuth tmpCred
  putStrLn $
    format ("To authorize flickr-promoter, open the following URL: " %
             s % "\n\nWhen you complete authorisation, copy the URL from the address bar here:\n")
    authorizationUrl

  authorizedUrl <- URI.mkURI <$> getLine
  let getVerifier :: URI.QueryParam -> Maybe ByteString
      getVerifier (URI.QueryParam k v) =
        if Just k == URI.mkQueryKey "oauth_verifier"
        then Just $ encodeUtf8 $ URI.unRText v
        else Nothing
      getVerifier _                                   = Nothing
  case mapMaybe getVerifier =<< (URI.uriQuery <$> authorizedUrl) of
    (verifierParam:_) -> getAccessToken flickrOAuth (injectVerifier verifierParam tmpCred) mgr
    []                -> error "No oauth_verifier parameter found in the URL copied. Make sure you copy it correctly."

main :: IO ()
main = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr flickrApi

  accessToken <- case persistedAccessToken of
    Nothing -> auth mgr
    Just t -> return t

  putStrLn $ format ("Using access token " % s) $ tshow accessToken

  (print =<<) $ runClientM (testLogin (Just apiKey)) env

  (print =<<) $ runClientM (photosGetInfo (Just apiKey) (Just "28168961808")) env
