{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | So the plan is:
--
-- Fetch "my" list of photos:
--
-- https://www.flickr.com/services/api/flickr.people.getPhotos.html
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
module Main where

import ClassyPrelude hiding (any)
import Control.Monad.Fail
import Control.Monad.Logger
import Data.Aeson
import Data.Binary.Builder hiding (empty)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import Data.Digest.Pure.SHA
import Data.Proxy
import Data.Scientific
import Data.Text.Read
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
import Turtle.Format (d, format, s, (%))
import Web.Authenticate.OAuth

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

newtype Location = Location {unLocation :: Text}
  deriving (Show)

data Photo = Photo
  { id :: PhotoId,
    title :: Text,
    tags :: Set Tag,
    groups :: Set GroupId,
    location :: Location,
    faves :: Word
  }
  deriving (Show)

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

  clientWithRoute pm _ =
    clientWithRoute pm (Proxy :: Proxy api)
      . appendToQueryString "nojsoncallback" (Just "1")
      . appendToQueryString "format" (Just "json")

  hoistClientMonad pm _ f cl =
    hoistClientMonad pm (Proxy :: Proxy api) f cl

data FlickrContent = FlickrContent
  {_content :: Text}
  deriving (Generic, FromJSON, Show)

data FlickrUser = FlickrUser
  {username :: FlickrContent}
  deriving (Generic, FromJSON, Show)

data LoginResponse = LoginResponse
  {user :: FlickrUser}
  deriving (Generic, FromJSON, Show)

data PhotoResponse = PhotoResponse
  {photo :: FlickrPhoto}
  deriving (Generic, FromJSON, Show)

data FlickrLocation = FlickrLocation
  { country :: FlickrContent,
    region :: FlickrContent,
    county :: Maybe FlickrContent,
    locality :: FlickrContent
  }
  deriving (Generic, FromJSON, Show)

extractLocation :: FlickrLocation -> Location
extractLocation loc =
  Location $
    intercalate
      ", "
      [ loc & country & _content,
        loc & region & _content,
        fromMaybe "" (loc & county & fmap _content),
        loc & locality & _content
      ]

data FlickrPhoto = FlickrPhoto
  { location :: FlickrLocation,
    tags :: FlickrTags
  }
  deriving (Generic, FromJSON, Show)

data FlickrTags = FlickrTags
  {tag :: [FlickrContent]}
  deriving (Generic, FromJSON, Show)

extractTags :: FlickrTags -> Set Tag
extractTags (FlickrTags tags) = setFromList $ map (Tag . _content) tags

data GetPhotosResponse = GetPhotosResponse
  { photos :: FlickrPhotos
  }
  deriving (Generic, FromJSON, Show)

data FlickrPhotoDigest = FlickrPhotoDigest
  { id :: PhotoId,
    views :: Maybe WordFromString,
    title :: Text,
    description :: FlickrContent
    -- We're not requesting tags or geo here as it's not formatted in
    -- a structured way in recentlyUpdated (tags is just a string and
    -- geo only has lon/lat, not locality name)
  }
  deriving (Generic, FromJSON, Show)

newtype BoolFromBit = BoolFromBit Bool
  deriving (Show)

instance FromJSON BoolFromBit where
  parseJSON (Number 1) = pure $ BoolFromBit True
  parseJSON (Number _) = pure $ BoolFromBit False
  parseJSON _ = fail "Could not parse BoolFromBit"

newtype WordFromString = WordFromString {unWordFromString :: Word}
  deriving (Show)

instance FromJSON WordFromString where
  parseJSON = withText "WordFromString" $ \text ->
    case decimal text of
      Right (parsed, "") -> pure $ WordFromString parsed
      _ -> fail "Could not parse WordFromString"

data FlickrPhotos = FlickrPhotos
  { photo :: [FlickrPhotoDigest],
    page :: Word,
    pages :: Word,
    perpage :: Word,
    total :: WordFromString
  }
  deriving (Generic, FromJSON, Show)

newtype CommaSeparatedList a = CSL [a]
  deriving (Show, Generic, Foldable, Functor)

instance ToHttpApiData a => ToHttpApiData (CommaSeparatedList a) where
  toQueryParam (CSL params) = intercalate "," $ map toQueryParam params

data FlickrPool = FlickrPool
  { id :: GroupId
  }
  deriving (Generic, FromJSON, Show)

data GetAllContextsResponse = GetAllContextsResponse
  { pool :: Maybe [FlickrPool]
  }
  deriving (Generic, FromJSON, Show)

extractGroups :: GetAllContextsResponse -> Set GroupId
extractGroups (GetAllContextsResponse Nothing) = mempty
extractGroups (GetAllContextsResponse (Just pools)) =
  map (getField @"id") pools & setFromList

data FlickrContentType = PhotosOnly

instance ToHttpApiData FlickrContentType where
  toQueryParam PhotosOnly = "1"

data FlickrPrivacyFilter = Public -- TODO | Friends | Family | FriendsAndFamily | Private

instance ToHttpApiData FlickrPrivacyFilter where
  toQueryParam Public = "1"

data Status = Ok | Fail deriving (Generic, Show)

instance FromJSON Status where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = camelTo2 '_'}

data PoolResponseCode = GroupLimit | UnknownCode Scientific deriving (Show)

instance FromJSON PoolResponseCode where
  parseJSON = withScientific "PoolResposeCode" $ \case
    5 -> pure GroupLimit
    o -> pure $ UnknownCode o

data PoolsAddResponse = PoolsAddResponse
  { stat :: Status,
    code :: Maybe PoolResponseCode
  }
  deriving (Generic, FromJSON, Show)

data PhotoFavoritesResponse = PhotoFavoritesResponse
  {total :: WordFromString}
  deriving (Generic, FromJSON)

-- TODO Client functions must have tagged arguments automatically, not
-- blind `ty` from `QueryParam lab ty`
--
-- TODO Combine supplying api_key with oauth helper?
--
-- Unauthenticated requests require api_key parameter, however it's
-- not actually required for those signed with OAuth 1.0a despite the
-- API documentation saying so.
type FlickrAPI =
  FlickrResponseFormat
    :> ( FlickrMethod "flickr.test.login" :> AuthProtect "oauth" :> Get '[JSON] LoginResponse
           :<|> FlickrMethod "flickr.people.getPhotos" :> QueryParam "user_id" UserId :> QueryParam "extras" (CommaSeparatedList Text) :> QueryParam "content_type" FlickrContentType :> QueryParam "privacy_filter" FlickrPrivacyFilter :> QueryParam "per_page" Word :> QueryParam "page" Word :> AuthProtect "oauth" :> Get '[JSON] GetPhotosResponse
           :<|> FlickrMethod "flickr.groups.pools.add" :> QueryParam "photo_id" PhotoId :> QueryParam "group_id" GroupId :> AuthProtect "oauth" :> Get '[JSON] PoolsAddResponse
           :<|> QueryParam "api_key" Text :> FlickrMethod "flickr.photos.getAllContexts" :> QueryParam "photo_id" PhotoId :> Get '[JSON] GetAllContextsResponse
           :<|> QueryParam "api_key" Text :> FlickrMethod "flickr.photos.getInfo" :> QueryParam "photo_id" PhotoId :> Get '[JSON] PhotoResponse
           :<|> QueryParam "api_key" Text :> FlickrMethod "flickr.photos.getFavorites" :> QueryParam "photo_id" PhotoId :> Get '[JSON] PhotoFavoritesResponse
       )

testLogin :<|> peopleGetPhotos :<|> poolsAdd :<|> photosGetAllContexts :<|> photosGetInfo :<|> photosGetFavorites = client (Proxy :: Proxy FlickrAPI)

-- TODO Wrap raw methods in something that will automatically provide
-- api_key

rules :: [Rule]
rules =
  [ any .=> "769299@N22",
    locatedIn "Prague" .=> "48889111127@N01",
    locatedIn "Bavaria" .=> "860590@N23",
    locatedIn "Crimea" .=> "60453939@N00",
    locatedIn "Lyon" .=> "13409106@N00"
  ]

flickrApi :: BaseUrl
flickrApi = BaseUrl Https "www.flickr.com" 443 "/services/rest/"

-- TODO Is this superseded by
-- &oauth_consumer_key=653e7a6ecc1d528c516cc8f92cf98611 in API
-- requests? https://www.flickr.com/services/api/auth.oauth.html
apiKey :: Text
apiKey = "53eeb65b3ecfc822e4cdfa8440e058fd"

apiSecret :: ByteString
apiSecret = "2f5d176193666a48"

flickrOAuth :: OAuth
flickrOAuth =
  newOAuth
    { oauthServerName = "Flickr",
      -- URLs from https://www.flickr.com/services/api/auth.oauth.html
      oauthRequestUri = "https://www.flickr.com/services/oauth/request_token",
      oauthAuthorizeUri = "https://www.flickr.com/services/oauth/authorize",
      oauthAccessTokenUri = "https://www.flickr.com/services/oauth/access_token",
      oauthConsumerKey = encodeUtf8 apiKey,
      -- Secret from https://www.flickr.com/services/apps/by/...
      oauthConsumerSecret = apiSecret,
      oauthCallback = Just "https://gist.github.com/dzhus/0bf2a8b1990c288315411ce69bca56df"
    }

-- Results from authorize URL redirect
persistedAccessToken :: Maybe Credential
persistedAccessToken = Just Credential {unCredential = [("fullname", "Dmitry Djouce"), ("oauth_token", "72157714614929972-9e8de28f8f2bf657"), ("oauth_token_secret", "4366f00aefaa68dd"), ("user_nsid", "46721940@N00"), ("username", "Dmitry Djouce")]}

-- | Request OAuth 1.0a authorisation with Flickr.
auth :: Manager -> IO Credential
auth mgr = do
  tmpCred <- getTemporaryCredential flickrOAuth mgr

  let authorizationUrl = format (s % "&perms=write") $ fromString $ authorizeUrl flickrOAuth tmpCred
  putStrLn $
    format
      ( "To authorize flickr-promoter, open the following URL: "
          % s
          % "\n\nWhen you complete authorisation, copy the URL from the address bar here:\n"
      )
      authorizationUrl

  mkURI <$> getLine >>= \case
    Just authorizedUrl ->
      case authorizedUrl ^. uriQuery ^? queryParam [queryKey|oauth_verifier|] of
        Just verifierParam -> getAccessToken flickrOAuth (injectVerifier (encodeUtf8 $ verifierParam ^. unRText) tmpCred) mgr
        Nothing -> error "No oauth_verifier parameter found in the URL copied. Make sure you copy it correctly."
    Nothing -> error "Could not parse the URL copied. Make sure you copy it correctly."

-- | Generate OAuth 1.0a signature base string as per
-- <https://oauth.net/core/1.0a/#anchor13>.
oauthBaseString ::
  ClientEnv ->
  Request ->
  LByteString
oauthBaseString env req =
  fromStrict $
    intercalate
      "&"
      [ requestMethod req,
        ( (baseUrl env & showBaseUrl & BS.pack)
            <> (requestPath req & toLazyByteString & toStrict)
        )
          & urlEncode True,
        requestQueryString req & toList & sort & renderQuery False & urlEncode True
      ]

-- | Sign a request as per <https://oauth.net/core/1.0a/#anchor15>. We
-- only support HMAC-SHA1 signatures
generateSignature ::
  OAuth ->
  Credential ->
  ClientEnv ->
  Request ->
  ByteString
generateSignature oa cred env req =
  case (oauthSignatureMethod oa, lookup "oauth_token_secret" $ unCredential cred) of
    (HMACSHA1, Just tokenSecret) ->
      B64.encode $ toStrict $ bytestringDigest $ hmacSha1 (fromStrict key) (oauthBaseString env req)
      where
        key = BS.intercalate "&" $ map paramEncode [oauthConsumerSecret oa, tokenSecret]
    (_, Nothing) -> error "Can't sign a request without oauth_token_secret in credential"
    _ -> error "Unsupported signature method"

signRequest ::
  OAuth ->
  Credential ->
  ClientEnv ->
  -- | @oauth_nonce@
  Text ->
  -- | @oauth_timestamp@
  POSIXTime ->
  Request ->
  Request
signRequest oa cred env nonce ts req =
  case lookup "oauth_token" $ unCredential cred of
    Just oauth_token ->
      req' & add "oauth_signature" (decodeUtf8 $ generateSignature oa cred env req')
      where
        -- https://oauth.net/core/1.0a/#anchor12
        add f v = appendToQueryString f (Just v)
        req' =
          req
            & add "oauth_consumer_key" (decodeUtf8 $ oauthConsumerKey oa)
            & add "oauth_nonce" nonce
            & add "oauth_signature_method" "HMAC-SHA1"
            & add "oauth_timestamp" (tshow (round (nominalDiffTimeToSeconds ts) :: Integer))
            & add "oauth_token" (decodeUtf8 oauth_token)
            & add "oauth_version" "1.0"
    Nothing -> error "Can't sign a request without oauth_token in credential"

-- TODO Figure out what is AuthClientData if it varies with each
-- request and needs IO
type instance AuthClientData (AuthProtect "oauth") = ()

-- | servant-client interface for OAuth 1.0a
runOAuthenticated ::
  OAuth ->
  Credential ->
  (AuthenticatedRequest (AuthProtect "oauth") -> ClientM r) ->
  ClientEnv ->
  IO (Either ClientError r)
runOAuthenticated oa cred act env = do
  nonce <- replicateM 10 $ randomRIO ('a', 'z')
  ts <- getPOSIXTime
  let sign = signRequest oa cred env nonce ts
      authenticator = mkAuthenticatedRequest () (\_ r -> sign r)
  runClientM (act authenticator) env

gatherPhotoInfo :: Text -> FlickrPhotoDigest -> ClientM Photo
gatherPhotoInfo key fpd = do
  let photoId = fpd & getField @"id"
  PhotoResponse {..} <- photosGetInfo (Just key) (Just photoId)
  contexts <- photosGetAllContexts (Just key) (Just photoId)
  faves <- photosGetFavorites (Just key) (Just photoId)
  return $
    Photo
      photoId
      (fpd & getField @"title")
      (photo & getField @"tags" & extractTags)
      (extractGroups contexts)
      (photo & getField @"location" & extractLocation)
      (faves & getField @"total" & unWordFromString)

-- | Which groups to post this photo to
candidateGroups :: Photo -> Set GroupId
candidateGroups photo = setFromList $
  catMaybes $
    (flip map) rules $
      \(Rule (predicate, targetGroup)) ->
        if predicate photo && targetGroup `notElem` (groups photo)
          then Just targetGroup
          else Nothing

main :: IO ()
main = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr flickrApi
      me = UserId "me"
      photoCount = 10

  throttledGroups <- newTVarIO (setFromList [])
  postedCounter <- newTVarIO (0 :: Word)

  runStdoutLoggingT $ do
    accessToken <- case persistedAccessToken of
      Nothing -> liftIO $ auth mgr
      Just t -> return t

    -- (print =<<) $ runOAuthenticated flickrOAuth accessToken testLogin env

    Right latest <- liftIO $ runOAuthenticated flickrOAuth accessToken (peopleGetPhotos (Just me) (Just $ CSL ["views", "description"]) (Just PhotosOnly) (Just Public) (Just photoCount) (Just 0)) env
    let photoDigests = latest & photos & getField @"photo"
    logInfoN $ format ("Fetched " % d % " latest photos") (length photoDigests)

    photosWithInfo <- liftIO $ rights <$> mapConcurrently (\fpd -> runClientM (gatherPhotoInfo apiKey fpd) env) photoDigests
    logInfoN $ format ("Gathered details for " % d % " photos") (length photosWithInfo)

    -- TODO Try to split out servant-specific IO with polysemy

    forM_ photosWithInfo $ \p -> do
      let candidates = candidateGroups p
      when (not $ null candidates) $ do
        logDebugN $ format (s % "/" % s % " should be in groups: " % s) (getField @"title" p) (unPhotoId $ getField @"id" p) (intercalate ", " $ map tshow $ toList candidates)
        forM_ (toList candidates) $
          \c -> do
            -- TODO This can still fail if we get throttled on two
            -- concurrent requests to add to the same group
            throttled <- readTVarIO throttledGroups
            when (c `notMember` (throttled :: Set GroupId)) $ do
              resp <- liftIO $ runOAuthenticated flickrOAuth accessToken (poolsAdd (Just (p & getField @"id")) (Just c)) env
              case resp of
                Right (PoolsAddResponse Ok _) -> do
                  atomically $ modifyTVar' postedCounter (1 +)
                  logInfoN $ format ("Posted " % s % " to " % s) (tshow p) (tshow c)
                Right (PoolsAddResponse Fail (Just GroupLimit)) -> do
                  logWarnN $ format ("Reached group limit for " % s) (tshow c)
                  atomically $ modifyTVar' throttledGroups (insertSet c)
                other ->
                  logErrorN $
                    format
                      ("Unknown error posting " % s % " to " % s % ": " % s)
                      (tshow p)
                      (tshow c)
                      (tshow other)

    readTVarIO postedCounter >>= logInfoN . format ("Added " % d % " new photos to groups")
