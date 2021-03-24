{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module IgMng.LineNotify (
    notifyLine
) where

import           Control.Monad.Extra         (ifM)
import qualified Data.ByteString             as B
import           Data.Functor                ((<&>))
import qualified Data.HashMap.Lazy           as HM
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.IO                as T
import           IgMng.IO                    (putStrLnErr)
import           IgMng.Network               (setRequestBearerAuth)
import           Network.HTTP.Client.Conduit (RequestBody (..))
import           Network.HTTP.Simple
import           System.Directory            (getHomeDirectory)
import           System.FilePath             ((</>))
import           Text.Toml                   (parseTomlDoc)
import           Text.Toml.Types             (Node (..))

lineNotifyAPIEndpoint :: String
lineNotifyAPIEndpoint = "https://notify-api.line.me/api/notify"

reqLineNotify :: B.ByteString -> B.ByteString -> Request
reqLineNotify token message = setRequestMethod "POST"
    $ setRequestBearerAuth token
    $ setRequestQueryString [("message", Just message)]
    $ parseRequest_ lineNotifyAPIEndpoint

loadLineNotifyToken :: FilePath -> IO (Maybe T.Text)
loadLineNotifyToken credentialFilePath = T.readFile credentialFilePath
    <&> parseTomlDoc credentialFilePath
    >>= \case
        Left err -> Nothing <$ putStrLnErr (show err)
        Right table -> maybe (Nothing <$ putStrLnErr errMsg) (pure . pure) $
            HM.lookup "line" table >>= \case
                VTable t -> HM.lookup "oauth_token" t >>= \case
                    VString oauthToken -> pure oauthToken
                    _ -> Nothing
                _ -> Nothing
    where
        errMsg = "expected the line notify api oauth_token, but not found"

notifyLine :: FilePath -> [T.Text] -> IO Bool
notifyLine credentialFilePath unfollowers = loadLineNotifyToken credentialFilePath >>= \case
    Nothing -> pure False
    Just oauthToken -> (==200)
        . getResponseStatusCode
        <$> httpLbs (reqLineNotify (T.encodeUtf8 oauthToken) message)
    where
        newLine = "\n"
        message = T.encodeUtf8 $ mconcat [
            newLine
          , "🚨 IG Unfollow detected 🚨"
          , newLine, newLine
          , T.intercalate newLine [ "・" <> uf <> ": https://instagram.com/" <> uf | uf <- unfollowers ]
          ]
