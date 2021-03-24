{-# LANGUAGE OverloadedStrings #-}
module IgMng.Network (
    setRequestBearerAuth
) where

import qualified Data.ByteString      as BS
import qualified Data.CaseInsensitive as CI
import           Network.HTTP.Conduit (requestHeaders)
import           Network.HTTP.Simple  (Request, setRequestHeaders)

buildBearerAuth :: BS.ByteString -> BS.ByteString
buildBearerAuth = BS.append "Bearer "

applyBearerAuth :: BS.ByteString -> Request -> Request
applyBearerAuth bearerToken req = setRequestHeaders (authHeader : requestHeaders req) req
    where
        authHeader = (CI.mk "Authorization", buildBearerAuth bearerToken)

setRequestBearerAuth :: BS.ByteString -> Request -> Request
setRequestBearerAuth = applyBearerAuth
