{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Promoter.FlickrAPI where

import ClassyPrelude hiding (any)
import Control.Monad.Fail
import Data.Aeson
import Data.Proxy
import Data.Scientific
import Data.Text.Read
import GHC.Records
import GHC.TypeLits
import Lens.Micro
import Promoter.Types
import Servant.API hiding (uriQuery)
import Servant.Client
import Servant.Client.Core

data FlickrMethod (a :: Symbol)

instance (KnownSymbol method, HasClient m api) => HasClient m (FlickrMethod method :> api) where
  type Client m (FlickrMethod method :> api) = Client m api

  clientWithRoute pm _ req = clientWithRoute pm (Proxy :: Proxy api) req'
    where
      req' = appendToQueryString "method" (Just (fromString $ symbolVal (Proxy :: Proxy method))) req

  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy :: Proxy api)

data FlickrResponseFormat = JsonFormat

instance HasClient m api => HasClient m (FlickrResponseFormat :> api) where
  type Client m (FlickrResponseFormat :> api) = Client m api

  clientWithRoute pm _ =
    clientWithRoute pm (Proxy :: Proxy api)
      . appendToQueryString "nojsoncallback" (Just "1")
      . appendToQueryString "format" (Just "json")

  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy :: Proxy api)

newtype FlickrContent = FlickrContent
  {_content :: Text}
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

newtype FlickrUser = FlickrUser
  {username :: FlickrContent}
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

newtype LoginResponse = LoginResponse
  {user :: FlickrUser}
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

newtype PhotoResponse = PhotoResponse
  {photo :: FlickrPhoto}
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

data FlickrLocation = FlickrLocation
  { country :: Maybe FlickrContent,
    region :: Maybe FlickrContent,
    county :: Maybe FlickrContent,
    locality :: Maybe FlickrContent
  }
  deriving (Generic, FromJSON, Show)

extractLocation :: FlickrLocation -> Location
extractLocation loc =
  [country, region, county, locality]
    & mapMaybe (\acc -> acc loc)
    & map _content
    & filter (/= "")
    & intercalate ", "
    & Location

data FlickrPhoto = FlickrPhoto
  { location :: Maybe FlickrLocation,
    tags :: FlickrTags
  }
  deriving (Generic, FromJSON, Show)

newtype FlickrTags = FlickrTags
  {tag :: [FlickrContent]}
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

extractTags :: FlickrTags -> Set Tag
extractTags (FlickrTags tags) = setFromList $ map (Tag . _content) tags

newtype GetPhotosResponse = GetPhotosResponse
  { photos :: FlickrPhotos
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

data Media = PhotoMedia | VideoMedia deriving (Generic, Eq, Show)

instance FromJSON Media where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = dropSuffix "_media" . camelTo2 '_'}

data FlickrPhotoDigest = FlickrPhotoDigest
  { id :: PhotoId,
    views :: Maybe WordFromString,
    title :: Text,
    description :: FlickrContent,
    media :: Media
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
    total :: Word
  }
  deriving (Generic, FromJSON, Show)

newtype FlickrPhotoFavorites = FlickrPhotoFavorites
  { total :: Word
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

newtype CommaSeparatedList a = CSL [a]
  deriving (Show, Generic, Foldable, Functor)

instance ToHttpApiData a => ToHttpApiData (CommaSeparatedList a) where
  toQueryParam (CSL params) = intercalate "," $ map toQueryParam params

newtype FlickrPool = FlickrPool
  { id :: GroupId
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

newtype GetAllContextsResponse = GetAllContextsResponse
  { pool :: Maybe [FlickrPool]
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

extractGroups :: GetAllContextsResponse -> Set GroupId
extractGroups (GetAllContextsResponse Nothing) = mempty
extractGroups (GetAllContextsResponse (Just pools)) =
  map (getField @"id") pools & setFromList

data FlickrContentTypes = PhotosOnly

instance ToHttpApiData FlickrContentTypes where
  toQueryParam PhotosOnly = "0"

data FlickrPrivacyFilter = Public -- TODO | Friends | Family | FriendsAndFamily | Private

instance ToHttpApiData FlickrPrivacyFilter where
  toQueryParam Public = "1"

data Status = Ok | Fail deriving (Generic, Show)

instance FromJSON Status where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = camelTo2 '_'}

data PoolResponseCode = GroupLimit | GroupNotFound | InappropriateContent | UnknownCode Scientific deriving (Show)

instance FromJSON PoolResponseCode where
  parseJSON = withScientific "PoolResposeCode" $ \case
    5 -> pure GroupLimit
    2 -> pure GroupNotFound
    8 -> pure InappropriateContent
    o -> pure $ UnknownCode o

data PoolsAddResponse = PoolsAddResponse
  { stat :: Status,
    code :: Maybe PoolResponseCode
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

newtype PhotoFavoritesResponse = PhotoFavoritesResponse
  { photo :: FlickrPhotoFavorites
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

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
           :<|> FlickrMethod "flickr.people.getPhotos" :> QueryParam "user_id" UserId :> QueryParam "extras" (CommaSeparatedList Text) :> QueryParam "content_types" FlickrContentTypes :> QueryParam "privacy_filter" FlickrPrivacyFilter :> QueryParam "per_page" Word :> QueryParam "page" Word :> AuthProtect "oauth" :> Get '[JSON] GetPhotosResponse
           :<|> FlickrMethod "flickr.groups.pools.add" :> QueryParam "photo_id" PhotoId :> QueryParam "group_id" GroupId :> AuthProtect "oauth" :> Get '[JSON] PoolsAddResponse
           :<|> QueryParam "api_key" Text :> FlickrMethod "flickr.photos.getAllContexts" :> QueryParam "photo_id" PhotoId :> Get '[JSON] GetAllContextsResponse
           :<|> QueryParam "api_key" Text :> FlickrMethod "flickr.photos.getInfo" :> QueryParam "photo_id" PhotoId :> Get '[JSON] PhotoResponse
           :<|> QueryParam "api_key" Text :> FlickrMethod "flickr.photos.getFavorites" :> QueryParam "photo_id" PhotoId :> Get '[JSON] PhotoFavoritesResponse
       )

-- TODO Wrap raw methods in something that will automatically provide
-- api_key
testLogin :<|> peopleGetPhotos :<|> poolsAdd :<|> photosGetAllContexts :<|> photosGetInfo :<|> photosGetFavorites = client (Proxy :: Proxy FlickrAPI)

flickrApi :: BaseUrl
flickrApi = BaseUrl Https "www.flickr.com" 443 "/services/rest/"
