{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module IgMng.Database.Client (
    newClient
  , insertLog
  , deleteLog
  , deleteLogWithYearAgo
  , selectLatestDiffLog
) where

import           Control.Monad.Extra       (ifM)
import           Control.Monad.Loops       (unfoldM)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.UTF8      as BSU
import           Data.Char                 (isSpace)
import           Data.Maybe                (fromJust)
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import           Data.Time                 (LocalTime)
import           Data.Tuple.Extra          (both, second)
import           Data.Word                 (Word64)
import           Database.MySQL.Base
import           IgMng.Database.Type
import           IgMng.IgRouter            (IgMngFollower (..), IgMngResp (..))
import           IgMng.IO                  (putStrLnErr)
import qualified System.IO.Streams         as S

loadConnInfo :: FilePath -> IO (Maybe ConnectInfo)
loadConnInfo fpath = do
    env <- map (both (filter (not . isSpace)) . second tail . break (=='=')) . lines
        <$> readFile fpath
    pure $ do
        mdb <- lookup "MYSQL_DATABASE" env
        mus <- lookup "MYSQL_USER" env
        mpw <- lookup "MYSQL_PASSWORD" env
        pure $ defaultConnectInfo {
            ciDatabase = BSU.fromString mdb
          , ciUser = BSU.fromString mus
          , ciPassword = BSU.fromString mpw
          }

newClient :: FilePath -> IO (Maybe MySQLConn)
newClient fpath = loadConnInfo fpath >>= \case
    Nothing -> Nothing <$ putStrLnErr "igmng.loadConnInfo: fail to load .env file"
    Just conn -> Just <$> connect conn

insertLog :: MySQLConn -> LocalTime -> IgMngResp -> IO OK
insertLog conn localtime resp =
    execute conn q
        [ toMySQLV $ followeesNum resp
        , toMySQLV $ followersNum resp
        , toMySQLV localtime
        , toMySQLV $ followers resp
        ]
    where
        q = "INSERT INTO follow_log(followees_num,followers_num,logged_date,followers) VALUES(?,?,?,?)"

selectLatestDiffLog :: MySQLConn -> IO (Maybe (IgMngResp, IgMngResp))
selectLatestDiffLog conn = do
    logs <- snd <$> query_ conn q
    igmngs <- unfoldM $ ifM (S.atEOF logs) (pure Nothing) $ do
        Just row <- S.read logs
        -- follow_log table has 5 cells
        if length row /= 5 then pure Nothing else do
            let MySQLInt32U followeesN = row !! 1
                MySQLInt32U followersN = row !! 2
                MySQLText json = row !! 4
            case eitherDecode $ BL.fromStrict $ encodeUtf8 json of
                Left e -> putStrLn e >> pure Nothing
                Right jsonSt -> pure $ Just $
                    IgMngResp (fromIntegral followeesN) (fromIntegral followersN) jsonSt
    -- expect two results
    if length igmngs /= 2 then pure Nothing else pure $ Just (head igmngs, igmngs !! 1)
    where
        q = "SELECT * FROM follow_log ORDER BY logged_date DESC LIMIT 2"

deleteLog :: MySQLConn -> Word64 -> IO OK
deleteLog conn ln = execute conn q [ toMySQLV ln ]
    where
        q = "DELETE FROM follow_log LIMIT ?"

deleteLogWithYearAgo :: MySQLConn -> Word64 -> LocalTime -> IO OK
deleteLogWithYearAgo conn ln ld = execute conn q [ toMySQLV ld, toMySQLV ln ]
    where
        q = "DELETE FROM follow_log WHERE logged_date <= ? ORDER BY logged_date LIMIT ?"
