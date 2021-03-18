{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import           Control.Monad         (when)
import           Data.Time             (UTCTime (..), getCurrentTime,
                                        getCurrentTimeZone, utcToLocalTime)
import           Database.MySQL.Base   (OK (..))
import           IgMng.Database.Client
import           IgMng.IgRouter

registerLog :: FilePath -> IO ()
registerLog fpath = requestFollowers "localhost" 3000 >>= \case
    Nothing -> putStrLn "parse failed"
    Just rs -> newClient fpath >>= \case
        Nothing -> fail ".env file load error"
        Just cli -> do
            time <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
            oar <- okAffectedRows <$> insertLog cli time rs
            when (oar /= 1) $ putStrLn "failed to register followers"

--main = registerLog
main :: IO ()
main = newClient "./containers/.env" >>= \case
    Nothing -> fail ".env file load error"
    Just cli -> selectLatestDiffLog cli >>= \case
        Nothing -> putStrLn "There is no comparison target"
        Just latest -> if uncurry (==) latest then putStrLn "no update" else
            putStrLn "update found"
