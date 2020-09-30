module Main where

import ClassyPrelude hiding (any)
import Control.Monad.Logger
import Data.Binary.Builder hiding (empty)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import Data.Digest.Pure.SHA
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Records
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

import Promoter.FlickrAPI
import Promoter.Processing
import Promoter.Types

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
persistedAccessToken = Just Credential {unCredential = [("fullname","Dmitry Djouce"),("oauth_token","72157716045986163-78e9b05ad8b18b40"),("oauth_token_secret","04a88aaa5c872e0c"),("user_nsid","46721940@N00"),("username","Dmitry Djouce")]}

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
      (faves & getField @"photo" & getField @"total" & unWordFromString)

main :: IO ()
main = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr flickrApi
      me = UserId "me"
      -- This is our own per-group posting limit
      photosPerGroup = 5
      -- How many latest photos to fetch
      maxPhotoCount = 500

  -- Map from group IDs to "how many more photos can we post to that
  -- group". Thus we can capture our own user-defined limits and
  -- per-group posting limits.
  groupLimits <- newTVarIO (mapFromList [] :: Map GroupId Word)
  postedCounter <- newTVarIO (0 :: Word)

  runStdoutLoggingT $ do
    accessToken <- case persistedAccessToken of
      Nothing -> liftIO $ auth mgr
      Just t -> return t

    -- (print =<<) $ runOAuthenticated flickrOAuth accessToken testLogin env

    Right latest <- liftIO $ runOAuthenticated flickrOAuth accessToken (peopleGetPhotos (Just me) (Just $ CSL ["views", "description", "media"]) (Just PhotosOnly) (Just Public) (Just maxPhotoCount) (Just 0)) env
    -- Filter out videos as we don't want to post them to any groups
    let photoDigests = latest & photos & getField @"photo" & filter ((== PhotoMedia) . media)
    logInfoN $ format ("Fetched " % d % " latest photos") (length photoDigests)

    photosWithInfo <- liftIO $ rights <$> mapConcurrently (\fpd -> runClientM (gatherPhotoInfo apiKey fpd) env) photoDigests
    logInfoN $ format ("Gathered details for " % d % " photos") (length photosWithInfo)

    -- TODO Try to split out servant-specific IO with polysemy

    forM_ (sortOn (Down . faves) photosWithInfo) $ \p -> do
      let candidates = candidateGroups p
      when (not $ null candidates) $ do
        logDebugN $
          format
            (s % "/" % s % " should be in groups: " % s)
            (getField @"title" p)
            (unPhotoId $ getField @"id" p)
            (intercalate ", " $ map tshow $ toList candidates)
        forM_ (toList candidates) $
          \c -> do
            -- TODO This can still fail if we get throttled on two
            -- concurrent requests to add to the same group
            canPost <- atomically $ do
              gl <- readTVar groupLimits
              case lookup c gl of
                Just r -> return (r > 0)
                Nothing -> return True
            when canPost $ do
              resp <- liftIO $ runOAuthenticated flickrOAuth accessToken (poolsAdd (Just (p & getField @"id")) (Just c)) env
              case resp of
                Right (PoolsAddResponse Ok _) -> do
                  atomically $ modifyTVar' postedCounter (1 +)
                  -- Update how many more photos can we post to this group
                  atomically $ modifyTVar' groupLimits $ \gl ->
                    insertMap c ((fromMaybe photosPerGroup $ lookup c gl) - 1) gl
                  logInfoN $ format ("Posted " % s % " to " % s) (tshow p) (tshow c)
                Right (PoolsAddResponse Fail (Just err)) -> do
                  logWarnN $ format ("Error posting to group " % s % ": " % s) (tshow c) (tshow err)
                  atomically $ modifyTVar' groupLimits (insertMap c 0)
                other ->
                  logErrorN $
                    format
                      ("Unknown error posting " % s % " to " % s % ": " % s)
                      (tshow p)
                      (tshow c)
                      (tshow other)

    readTVarIO postedCounter >>= logInfoN . format ("Added " % d % " new photos to groups")
    readTVarIO groupLimits >>= \limits -> do
      let depleted = filter (not . (> 0) . snd) (mapToList limits)
      when (not $ null depleted) $
        logInfoN $
          format
            ("Posting limits reached for " % d % " groups: " % s)
            (length depleted)
            (tshow (map fst depleted))
