module Servant.Client.OAuth1 (authenticate, runOAuthenticated) where

import ClassyPrelude hiding (any)
import Data.Binary.Builder hiding (empty)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import Data.Digest.Pure.SHA
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Lens.Micro
import Network.HTTP.Types.URI
import Servant.API hiding (uriQuery)
import Servant.Client
import Servant.Client.Core
import System.Random
import Web.Authenticate.OAuth

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
