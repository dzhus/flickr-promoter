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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import ClassyPrelude hiding (any)

import Data.Aeson
import Data.Binary.Builder
import Data.Digest.Pure.SHA
import Data.Proxy
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Records
import GHC.TypeLits
import Lens.Micro
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS
import Network.HTTP.Types.URI
import Servant.API hiding (uriQuery)
import Servant.Client
import Servant.Client.Core
import System.Random
import Text.URI (mkURI)
import Text.URI.Lens
import Text.URI.QQ
import Turtle.Format (format, (%), s)
import Web.Authenticate.OAuth

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS

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

type FlickrAPI = FlickrResponseFormat :> (QueryParam "api_key" Text :> FlickrMethod "flickr.test.login" :>  AuthProtect "oauth" :> Get '[JSON] LoginResponse :<|>
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

  mkURI <$> getLine >>= \case
    Just authorizedUrl ->
      case authorizedUrl ^. uriQuery ^? queryParam [queryKey|oauth_verifier|] of
        Just verifierParam -> getAccessToken flickrOAuth (injectVerifier (encodeUtf8 $ verifierParam ^. unRText) tmpCred) mgr
        Nothing -> error "No oauth_verifier parameter found in the URL copied. Make sure you copy it correctly."
    Nothing -> error "Could not parse the URL copied. Make sure you copy it correctly."

-- | Generate OAuth 1.0a signature base string as per
-- https://oauth.net/core/1.0a/#anchor13.
oauthBaseString
  :: ClientEnv
  -> Request
  -> LByteString
oauthBaseString env req = fromStrict $ intercalate "&"
  [ requestMethod req
  , ((baseUrl env & showBaseUrl & BS.pack) <>
     (requestPath req & toLazyByteString & toStrict)) & urlEncode True
  , requestQueryString req & toList & sort & renderQuery False & urlEncode True
  ]

generateSignature
  :: OAuth
  -> Credential
  -> ClientEnv
  -> Request
  -> ByteString
generateSignature oa cred env req =
  case (oauthSignatureMethod oa, lookup "oauth_token_secret" $ unCredential cred) of
    (HMACSHA1, Just tokenSecret) ->
      B64.encode $ toStrict $ bytestringDigest $ hmacSha1 (fromStrict key) (oauthBaseString env req)
      where
        key = BS.intercalate "&" $ map paramEncode [oauthConsumerSecret oa, tokenSecret]
    (_, Nothing) -> error "Can't sign a request without oauth_token_secret in credential"
    _ -> error "Unsupported signature method"

signRequest
  :: OAuth
  -> Credential
  -> ClientEnv
  -> Text
  -- ^ @oauth_nonce@
  -> POSIXTime
  -- ^ @oauth_timestamp@
  -> Request
  -> Request
signRequest oa cred env nonce ts req =
  case lookup "oauth_token" $ unCredential cred of
    Just oauth_token ->
      req' & add "oauth_signature" (decodeUtf8 $ generateSignature oa cred env req')
      where
        -- https://oauth.net/core/1.0a/#anchor12
        add f v = appendToQueryString f (Just v)
        req' = req &
          add "oauth_consumer_key" (decodeUtf8 $ oauthConsumerKey oa) &
          add "oauth_nonce" nonce &
          add "oauth_signature_method" "HMAC-SHA1" &
          add "oauth_timestamp" (tshow (round (nominalDiffTimeToSeconds ts) :: Integer)) &
          add "oauth_token" (decodeUtf8 oauth_token) &
          add "oauth_version" "1.0"
    Nothing -> error "Can't sign a request without oauth_token in credential"

-- TODO Figure out what is AuthClientData if it varies with each
-- request and needs IO
type instance AuthClientData (AuthProtect "oauth") = ()

main :: IO ()
main = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr flickrApi

  accessToken <- case persistedAccessToken of
    Nothing -> auth mgr
    Just t -> return t

  putStrLn $ format ("Using access token " % s) $ tshow accessToken
  print flickrOAuth

  nonce <- replicateM 10 $ randomRIO ('a', 'z')
  ts <- getPOSIXTime

  -- The problem here is that authenticate-oauth uses Request type
  -- from http-client whereas servant authentication hooks expose
  -- servant-client's Request which is different from http-client.
  --
  -- A rewrite involves a new "base string" computation mechanism
  -- which is also more involved with servant-client, as requests do
  -- not have the full URL in them (hostname is provided separately as
  -- they are not considered part of the API).
  let sign = signRequest flickrOAuth accessToken env nonce ts
      authenticator = mkAuthenticatedRequest () (\_ r -> sign r)

  (print =<<) $ runClientM (testLogin (Just apiKey) authenticator) env

  (print =<<) $ runClientM (photosGetInfo (Just apiKey) (Just "28168961808")) env
