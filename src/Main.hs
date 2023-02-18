{-# LANGUAGE QuasiQuotes #-}

module Main where

import ClassyPrelude hiding (any)
import Control.Monad.Fail
import Control.Monad.Logger
import Data.Binary.Builder hiding (empty)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import Data.Digest.Pure.SHA
import qualified Data.Text.Encoding.Base64 as T64
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Monoid
import GHC.Records
import Lens.Micro
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS
import Network.HTTP.Types.URI
import Promoter.FlickrAPI
import Promoter.Rules
import Promoter.Types
import Servant.API hiding (uriQuery)
import Servant.Client
import Servant.Client.Core
import System.Envy hiding (env)
import System.Exit
import System.Random
import System.Random.Shuffle
import Text.Read
import Text.URI (mkURI)
import Text.URI.Lens
import Text.URI.QQ
import Turtle.Format (d, format, s, (%))
import Web.Authenticate.OAuth

flickrOAuth :: Text -> ByteString -> OAuth
flickrOAuth apiKey apiSecret =
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

-- | Request OAuth 1.0a authorisation with Flickr.
auth :: Manager -> OAuth -> IO Credential
auth mgr authConfig = do
  tmpCred <- getTemporaryCredential authConfig mgr

  let authorizationUrl = format (s % "&perms=write") $ fromString $ authorizeUrl authConfig tmpCred
  putStrLn $
    format
      ( "To authorize flickr-promoter, open the following URL: "
          % s
          % "\n\nWhen you complete authorisation, copy the URL from the address bar here:\n"
      )
      authorizationUrl

  res <- mkURI <$> getLine
  case res of
    Just authorizedUrl ->
      case authorizedUrl ^. uriQuery ^? queryParam [queryKey|oauth_verifier|] of
        Just verifierParam -> getAccessToken authConfig (injectVerifier (encodeUtf8 $ verifierParam ^. unRText) tmpCred) mgr
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
      B64.encodeBase64' $ toStrict $ bytestringDigest $ hmacSha1 (fromStrict key) (oauthBaseString env req)
      where
        key = BS.intercalate "&" $ map paramEncode [oauthConsumerSecret oa, tokenSecret]
    (_, Nothing) -> error "Can't sign a request without oauth_token_secret in credential"
    _ -> error "Unsupported signature method"

signRequest ::
  OAuth ->
  Credential ->
  ClientEnv ->
  -- | @oauth_nonce@
  ByteString ->
  -- | @oauth_timestamp@
  POSIXTime ->
  Request ->
  Request
signRequest oa cred env nonce ts req =
  case lookup "oauth_token" $ unCredential cred of
    Just oauth_token ->
      req' & add "oauth_signature" (generateSignature oa cred env req')
      where
        -- https://oauth.net/core/1.0a/#anchor12
        add f v = appendToQueryString f (Just v)
        req' =
          req
            & add "oauth_consumer_key" (oauthConsumerKey oa)
            & add "oauth_nonce" nonce
            & add "oauth_signature_method" "HMAC-SHA1"
            & add "oauth_timestamp" (encodeUtf8 $ tshow (round (nominalDiffTimeToSeconds ts) :: Integer))
            & add "oauth_token" oauth_token
            & add "oauth_version" "1.0"
    Nothing -> error "Can't sign a request without oauth_token in credential"

-- TODO Figure out what is AuthClientData if it varies with each
-- request and needs IO
type instance AuthClientData (AuthProtect "oauth") = ()

authenticate ::
  OAuth ->
  Credential ->
  ClientEnv ->
  (AuthenticatedRequest (AuthProtect "oauth") -> r) ->
  IO r
authenticate oa cred env act = do
  nonce <- encodeUtf8 <$> replicateM 10 (randomRIO ('a', 'z'))
  ts <- getPOSIXTime
  let sign = signRequest oa cred env nonce ts
      authenticator = mkAuthenticatedRequest () (\_ r -> sign r)
  return $ act authenticator

-- | servant-client interface for OAuth 1.0a
runOAuthenticated ::
  OAuth ->
  Credential ->
  (AuthenticatedRequest (AuthProtect "oauth") -> ClientM r) ->
  ClientEnv ->
  IO (Either ClientError r)
runOAuthenticated oa cred act env = do
  req <- liftIO $ authenticate oa cred env act
  runClientM req env

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
      (photo & getField @"location" & fmap extractLocation)
      (faves & getField @"photo" & getField @"total")

getLatestPhotos ::
  OAuth ->
  Credential ->
  ClientEnv ->
  UserId ->
  CommaSeparatedList Text ->
  FlickrContentType ->
  FlickrPrivacyFilter ->
  Int ->
  IO (Either ClientError [FlickrPhotoDigest])
getLatestPhotos authConfig accessToken env userId extras cType privacyFilter maxPhotos = do
  let -- as per https://www.flickr.com/services/api/flickr.people.getPhotos.html
      perPage = 500
      fetchFromPage page acc = do
        req <-
          liftIO $
            authenticate authConfig accessToken env $
              peopleGetPhotos
                (Just userId)
                (Just extras)
                (Just cType)
                (Just privacyFilter)
                (Just perPage)
                (Just page)

        latest <- req
        let photos' = latest & photos
            acc' = acc ++ (photos' & getField @"photo")
        if  -- We ran out of pages
            ((photos' & getField @"page") == (photos' & getField @"pages"))
            ||
            -- If actual number of photos is larger than reported
            (maxPhotos <= length acc')
            -- This would be the last page we'd want to fetch,
            -- assuming numbers reported are correct
            || (fromIntegral maxPhotos <= (perPage * (photos' & getField @"page")))
          then return $ take maxPhotos acc'
          else fetchFromPage (page + 1) acc'
  runClientM (fetchFromPage 0 []) env

data Config = Config
  { apiKey :: Maybe Text,
    apiSecret :: Maybe ByteString,
    accessToken :: Maybe PersistedCredential
  }
  deriving (Generic, Show)

instance FromEnv Config where
  fromEnv = gFromEnvCustom defOption {customPrefix = "FLICKR_PROMOTER"}

newtype PersistedCredential = PersistedCredential Credential deriving (Read, Show)

instance Var PersistedCredential where
  fromVar t = case T64.decodeBase64 (pack t) of
    Left _ -> fail "Could not parse PersistedCredential"
    Right v -> Just (PersistedCredential $ read $ unpack v)
  toVar (PersistedCredential t) = unpack $ T64.encodeBase64 $ tshow t

main :: IO ()
main = do
  appCfg <- decodeWithDefaults (Config Nothing Nothing Nothing)
  case appCfg of
    Config (Just key) (Just secret) cred ->
      let authConfig = flickrOAuth key secret
       in do
            mgr <- newTlsManager
            case cred of
              Nothing -> do
                newToken <- liftIO $ auth mgr authConfig
                putStrLn $ format ("Now run with FLICKR_PROMOTER_ACCESS_TOKEN=" % s) (T64.encodeBase64 $ tshow newToken)
                exitWith (ExitFailure 1)
              Just (PersistedCredential t) -> do
                runStdoutLoggingT $ process authConfig mgr t
    _ -> error "Populate FLICKR_PROMOTER_API_KEY and FLICKR_PROMOTER_API_SECRET from https://www.flickr.com/services/apps/by/..."

data API = API
  { authConfig :: OAuth
  , token :: Credential
  , env :: ClientEnv }

  -- | This is our own per-group posting limit
photosPerGroup :: Word
photosPerGroup = 5

data GroupInfo = GroupInfo { left :: Word, posted :: Word }

startedPosting :: GroupInfo
startedPosting = GroupInfo (photosPerGroup - 1) photosPerGroup

onePosted :: GroupInfo -> GroupInfo
onePosted GroupInfo{..} = GroupInfo (left - 1) (posted + 1)

neverPosted :: GroupInfo
neverPosted = GroupInfo 0 0

noneLeft :: GroupInfo -> GroupInfo
noneLeft GroupInfo{..} = GroupInfo 0 posted

type GroupLimits = Map GroupId GroupInfo

postToGroup
  :: (MonadFail m, MonadLoggerIO m)
  => API
  -> Photo
  -> GroupLimits
  -> GroupId
  -> m GroupLimits
postToGroup API{..} p groupLimits group = do
  resp <- liftIO $ runOAuthenticated authConfig token (poolsAdd (Just (p & getField @"id")) (Just group)) env
  case resp of
    Right (PoolsAddResponse Ok _) -> do
      logInfoN $ format ("Posted " % s % " to " % s) (tshow p) (tshow group)
      return $ alterMap (maybe (Just startedPosting) (Just . onePosted)) group groupLimits
    Right (PoolsAddResponse Fail (Just err)) -> do
      logWarnN $ format ("Error posting to group " % s % ": " % s) (tshow group) (tshow err)
      return $ alterMap (maybe (Just neverPosted) (Just . noneLeft)) group groupLimits
    other -> do
      logErrorN $
        format
        ("Unknown error posting " % s % " to " % s % ": " % s)
        (tshow p)
        (tshow group)
        (tshow other)
      return groupLimits

processPhoto
  :: (MonadFail m, MonadLoggerIO m)
  => API
  -> GroupLimits
  -> Photo
  -> m GroupLimits
processPhoto api groupLimits photo =
  case matchingGroups photo of
    [] -> return groupLimits
    groups -> do
      logDebugN $
        format
        (s % "/" % s % " should be in groups: " % s)
        (getField @"title" photo)
        (unPhotoId $ getField @"id" photo)
        (intercalate ", " $ map tshow $ toList groups)
      foldM (postToGroup api photo) groupLimits (toList groups)

foldMWith
  :: (MonoFoldable mono, Monad m)
  => a -> mono -> (a -> Element mono -> m a) -> m a
foldMWith acc coll f = foldM f acc coll

process :: (MonadFail m, MonadLoggerIO m) => OAuth -> Manager -> Credential -> m ()
process authConfig mgr token = do
  let me = UserId "me"
      env = mkClientEnv mgr flickrApi
      -- How many latest photos to fetch
      maxPhotoCount = 2000

  (logInfoN . tshow =<<) $ liftIO $ runOAuthenticated authConfig token testLogin env

  -- Map from group IDs to "how many more photos can we post to that
  -- group". Thus we can capture our own user-defined limits and
  -- per-group posting limits.
  let groupLimits = mapFromList [] :: Map GroupId GroupInfo
      api = API authConfig token env

  Right latest <-
    liftIO $
      getLatestPhotos authConfig token env me (CSL ["views", "description", "media"]) PhotosOnly Public maxPhotoCount
  -- Filter out videos as we don't want to post them to any groups
  let photoDigests = latest & filter ((== PhotoMedia) . media)
  logInfoN $ format ("Fetched " % d % " latest photos") (length photoDigests)

  photosWithInfo' <- liftIO $ pooledMapConcurrentlyN 100 (\fpd -> runClientM (gatherPhotoInfo (decodeUtf8 $ oauthConsumerKey authConfig) fpd) env) photoDigests

  let unparsed = lefts photosWithInfo'
  unless (null unparsed) $ do
    forM_ unparsed $ \up -> logErrorN (tshow up)
    liftIO $ exitWith (ExitFailure 1)

  photosWithInfo <- liftIO $ shuffleM $ rights photosWithInfo'
  logInfoN $ format ("Gathered details for " % d % " photos") (length photosWithInfo)

  finalGroupLimits <- foldM (processPhoto api) groupLimits photosWithInfo
  let totalPosted = getSum $ foldMap (Sum . posted) finalGroupLimits
      depleted = finalGroupLimits &
                 filterMap ((== 0) . left) &
                 keys

  logInfoN $ format ("Added " % d % " new photos to groups") totalPosted

  unless (null depleted) $
    logInfoN $
    format
    ("Posting limits reached for " % d % " groups: " % s)
    (length depleted)
    (tshow depleted)
