{-# LANGUAGE QuasiQuotes #-}

module Main where

import ClassyPrelude hiding (FilePath, any)
import Control.Concurrent
import Control.Monad.Fail
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV
import Data.Monoid
import qualified Data.Text.Encoding.Base64 as T64
import Data.Time.Clock (NominalDiffTime, diffUTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import GHC.Records
import Lens.Micro
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Numeric.Natural
import Promoter.FlickrAPI
import Promoter.Rules (matchingGroups)
import Promoter.Types
import Servant.Client
import Servant.Client.OAuth1
import System.Envy hiding (env)
import System.Exit
import System.Random.Shuffle
import Text.Read
import Text.URI (mkURI)
import Text.URI.Lens
import Text.URI.QQ
import Turtle (FilePath)
import Turtle.Format (d, format, s, (%))
import Turtle.Options as Options
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

gatherPhotoInfo :: Text -> FlickrPhotoDigest -> ClientM Photo
gatherPhotoInfo key fpd = do
  let photoId = fpd & getField @"id"
  PhotoResponse {..} <- photosGetInfo (Just key) (Just photoId)
  contexts <- photosGetAllContexts (Just key) (Just photoId)
  faves <- photosGetFavorites (Just key) (Just photoId)
  return $
    Photo
      { id = photoId,
        title = fpd & getField @"title",
        tags = photo & getField @"tags" & extractTags,
        groups = extractGroups contexts,
        location = photo & getField @"location" & fmap extractLocation,
        faves = faves & getField @"photo" & getField @"total",
        views = fpd & getField @"views" & maybe 0 unWordFromString
      }

getLatestPhotos ::
  OAuth ->
  Credential ->
  ClientEnv ->
  UserId ->
  FlickrPrivacyFilter ->
  Int ->
  IO (Either ClientError [FlickrPhotoDigest])
getLatestPhotos authConfig accessToken env userId privacyFilter maxPhotos = do
  let -- as per https://www.flickr.com/services/api/flickr.people.getPhotos.html
      perPage = 500
      fetchFromPage page acc = do
        req <-
          liftIO $
            authenticate authConfig accessToken env $
              peopleGetPhotos
                (Just userId)
                (Just $ CSL ["views", "description", "media"])
                (Just PhotosOnly)
                (Just Safe)
                (Just privacyFilter)
                (Just perPage)
                (Just page)

        latest <- req
        let photos' = latest & photos
            -- Filter by actual media type - looks like Flickr API
            -- still includes videos even when only photos are
            -- requested
            pagePhotos =
              photos'
                & getField @"photo"
                & filter ((== PhotoMedia) . media)
            acc' = acc ++ pagePhotos
        if -- We ran out of pages
        ((photos' & getField @"page") == (photos' & getField @"pages"))
          ||
          -- If actual number of photos is larger than requested
          (maxPhotos <= length acc')
          then return $ take maxPhotos acc'
          else fetchFromPage (page + 1) acc'
  runClientM (fetchFromPage 1 []) env

data Config = Config
  { apiKey :: Maybe Text,
    apiSecret :: Maybe ByteString,
    accessToken :: Maybe PersistedCredential
  }
  deriving (Generic, Show)

data Options = Options
  { reportFile :: Maybe FilePath,
    noPosting :: Bool
  }

optionsParser :: Options.Parser Options
optionsParser =
  Options
    <$> optional (optPath "report" 'r' "Path to CSV file to write photo views/faves statistics to")
    <*> switch "no-posting" 'd' "Do not actually post photos to any groups"

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
  opts <- options "flickr-promoter" optionsParser
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
                runStdoutLoggingT $ process authConfig mgr t opts
    _ -> error "Populate FLICKR_PROMOTER_API_KEY and FLICKR_PROMOTER_API_SECRET from https://www.flickr.com/services/apps/by/..."

data API = API
  { authConfig :: OAuth,
    token :: Credential,
    env :: ClientEnv
  }

-- \| This is our own per-group posting limit
photosPerGroup :: Word
photosPerGroup = 5

data GroupInfo = GroupInfo {left :: Word, posted :: Word}

startedPosting :: GroupInfo
startedPosting = GroupInfo (photosPerGroup - 1) 1

onePosted :: GroupInfo -> GroupInfo
onePosted GroupInfo {..} = GroupInfo (left - 1) (posted + 1)

neverPosted :: GroupInfo
neverPosted = GroupInfo 0 0

noneLeft :: GroupInfo -> GroupInfo
noneLeft GroupInfo {..} = GroupInfo 0 posted

type GroupLimits = Map GroupId GroupInfo

formatError :: (Show b) => Either ClientError b -> Text
formatError (Left (FailureResponse _req res)) =
  tshow $ statusCode $ responseStatusCode res
formatError x = tshow x

postToGroup ::
  (MonadFail m, MonadLoggerIO m) =>
  API ->
  ThrottlingQueue ->
  Photo ->
  GroupLimits ->
  GroupId ->
  m GroupLimits
postToGroup API {..} tq p groupLimits targetGroup = do
  case left <$> lookup targetGroup groupLimits of
    -- Per-group posting limit reached
    Just 0 -> return groupLimits
    _ -> do
      let addRequest = poolsAdd (Just (p & getField @"id")) (Just targetGroup)
          updateLimits addGroupLimit changeExistingGroup =
            return $ alterMap (maybe (Just addGroupLimit) (Just . changeExistingGroup)) targetGroup groupLimits
      resp <- liftIO $ throttle tq $ runOAuthenticated authConfig token addRequest env
      case resp of
        Right (PoolsAddResponse Ok _) -> do
          logInfoN $
            format
              ("Posted " % s % " to " % s)
              (tshow p)
              (tshow targetGroup)
          updateLimits startedPosting onePosted
        Right (PoolsAddResponse Fail (Just err)) -> do
          logWarnN $
            format
              ("Error posting " % s % " to group " % s % ": " % s)
              (tshow p)
              (tshow targetGroup)
              (tshow err)
          updateLimits neverPosted noneLeft
        other -> do
          logErrorN $
            format
              ("Unknown error posting " % s % " to " % s % ": " % s)
              (tshow p)
              (tshow targetGroup)
              (formatError other)
          return groupLimits

type ThrottlingQueue = TBQueue UTCTime

requestsPerUnit :: Natural
requestsPerUnit = 1

throttlingIntervalSeconds :: NominalDiffTime
throttlingIntervalSeconds = secondsToNominalDiffTime 5.0

throttle ::
  (MonadIO m) =>
  ThrottlingQueue ->
  m a ->
  m a
throttle tq action = do
  now <- liftIO getCurrentTime
  delta <- atomically $ do
    full <- isFullTBQueue tq
    oldest <- if full then tryPeekTBQueue tq else return Nothing
    case oldest of
      Nothing -> writeTBQueue tq now >> return Nothing
      Just oldestTs -> do
        let elapsed = diffUTCTime now oldestTs
            delta = elapsed - throttlingIntervalSeconds
        if delta < 0
          then return $ Just $ negate $ nominalDiffTimeToSeconds delta
          else readTBQueue tq >> writeTBQueue tq now >> return Nothing
  case delta of
    Just delayInSeconds -> do
      let delayInMicroseconds = floor $ toRational delayInSeconds * 1_000_000
      liftIO $ threadDelay delayInMicroseconds
      throttle tq action
    Nothing -> action

processPhoto ::
  (MonadFail m, MonadLoggerIO m) =>
  API ->
  ThrottlingQueue ->
  GroupLimits ->
  Photo ->
  m GroupLimits
processPhoto api throttlingQueue groupLimits photo =
  let groups = matchingGroups photo
   in if groups == mempty
        then return groupLimits
        else do
          logDebugN $
            format
              (s % "/" % s % " should be in groups: " % s)
              (getField @"title" photo)
              (unPhotoId $ getField @"id" photo)
              (intercalate ", " $ map tshow $ toList groups)
          foldM (postToGroup api throttlingQueue photo) groupLimits (toList groups)

process :: (MonadFail m, MonadLoggerIO m) => OAuth -> Manager -> Credential -> Options -> m ()
process authConfig mgr token Options {..} = do
  let me = UserId "me"
      env = mkClientEnv mgr flickrApi
      -- How many latest photos to fetch
      maxPhotoCount = 2000

  (logInfoN . formatError =<<) $ liftIO $ runOAuthenticated authConfig token testLogin env

  -- Map from group IDs to "how many more photos can we post to that
  -- group". Thus we can capture our own user-defined limits and
  -- per-group posting limits.
  let groupLimits = mapFromList [] :: Map GroupId GroupInfo
      api = API authConfig token env

  Right photoDigests <-
    liftIO $ getLatestPhotos authConfig token env me Public maxPhotoCount
  logInfoN $ format ("Fetched " % d % " latest photos") (length photoDigests)

  photosWithInfo' <- liftIO $ pooledMapConcurrentlyN 100 (\fpd -> runClientM (gatherPhotoInfo (decodeUtf8 $ oauthConsumerKey authConfig) fpd) env) photoDigests

  let unparsed = lefts photosWithInfo'
  unless (null unparsed) $ do
    forM_ unparsed $ \up -> logErrorN (tshow up)
    liftIO $ exitWith (ExitFailure 1)

  photosWithInfo <- liftIO $ shuffleM $ rights photosWithInfo'
  logInfoN $ format ("Gathered details for " % d % " photos") (length photosWithInfo)

  case reportFile of
    Just fp -> do
      liftIO $
        LBS.writeFile fp $
          CSV.encodeDefaultOrderedByName photosWithInfo
      logInfoN $ fromString $ "Wrote photo stats report to " ++ fp
    Nothing -> return ()

  throttlingQueue <- atomically $ newTBQueue requestsPerUnit

  finalGroupLimits <- if noPosting then return groupLimits else foldM (processPhoto api throttlingQueue) groupLimits photosWithInfo
  let totalPosted = getSum $ foldMap (Sum . posted) finalGroupLimits
      depleted =
        finalGroupLimits
          & filterMap ((== 0) . left)
          & keys

  logInfoN $ format ("Added " % d % " new photos to groups") totalPosted

  unless (null depleted) $
    logInfoN $
      format
        ("Posting limits reached for " % d % " groups: " % s)
        (length depleted)
        (tshow depleted)
