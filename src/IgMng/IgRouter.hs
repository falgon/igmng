{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings,
             TypeSynonymInstances #-}
module IgMng.IgRouter (
    IgMngFollower (..)
  , IgMngResp (..)
  , requestFollowers
) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Functor         ((<&>))
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Data.Word            (Word64)
import           Database.MySQL.Base  (MySQLValue (..))
import           GHC.Generics         (Generic)
import           IgMng.Database.Type  (MySQLType (..))
import           Network.HTTP.Simple  (getResponseBody, httpLbs, parseRequest,
                                       setRequestPort)

data IgMngFollower = IgMngFollower {
    userId :: Word64
  , name   :: T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON IgMngFollower where
    parseJSON (Object v) = IgMngFollower <$> v .: "user_id" <*> v .: "name"
    parseJSON _          = mempty

instance ToJSON IgMngFollower where
    toEncoding (IgMngFollower u n) = pairs ("user_id" .= u <> "name" .= n)

instance MySQLType IgMngFollower where
    toMySQLV = MySQLText . T.decodeUtf8 . BL.toStrict . encode

type IgMngFollowers = [IgMngFollower]

instance MySQLType IgMngFollowers where
    toMySQLV = MySQLText . T.decodeUtf8 . BL.toStrict . encode

data IgMngResp = IgMngResp {
    followeesNum :: Word64
  , followersNum :: Word64
  , followers    :: [IgMngFollower]
  } deriving (Show, Eq, Generic)

instance FromJSON IgMngResp where
    parseJSON (Object v) = IgMngResp
        <$> v .: "followees_num"
        <*> v .: "followers_num"
        <*> v .: "followers"
    parseJSON _ = mempty

instance ToJSON IgMngResp where
    toEncoding (IgMngResp ee er s) = pairs $ mconcat [
        "followees_num" .= ee
      , "followers_num" .= er
      , "followers" .= s
      ]

instance MySQLType IgMngResp where
    toMySQLV = MySQLText . T.decodeUtf8 . BL.toStrict . encode

requestFollowers :: String -> Int -> IO (Maybe IgMngResp)
requestFollowers host port = parseRequest ("http://" <> host <> "/follow")
    >>= httpLbs . setRequestPort port
    <&> decode . getResponseBody
