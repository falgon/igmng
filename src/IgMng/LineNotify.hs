{-# LANGUAGE OverloadedStrings #-}
module IgMng.LineNotify (
    notifyLine
  , reqLineNotify
) where

import Network.HTTP.Simple
import Network.HTTP.Client.Conduit (RequestBody (..))
import qualified Data.ByteString as B

lineNotifyAPIEndpoint :: String
lineNotifyAPIEndpoint = "https://notify-api.line.me/api/notify"

reqLineNotify :: B.ByteString -> B.ByteString -> Request
reqLineNotify token message = setRequestMethod "POST"
    $ setRequestBearerAuth token
    $ setRequestBody (RequestBodyBS $ "message=" <> message)
    $ parseRequest_ lineNotifyAPIEndpoint

notifyLine = undefined
