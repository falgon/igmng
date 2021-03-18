{-# LANGUAGE ExplicitNamespaces, LambdaCase, OverloadedStrings, Rank2Types,
             TypeOperators #-}
module Main where

import           Control.Monad             (when)
import           Control.Monad             (unless)
import           Control.Monad             (MonadPlus (..))
import           Control.Monad.Extra       (whenM)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Control.Natural           (type (~>))
import           Data.Maybe                (isJust)
import           Data.Time                 (UTCTime (..), getCurrentTime,
                                            getCurrentTimeZone, utcToLocalTime)
import           Database.MySQL.Base       (MySQLConn, OK (..))
import           IgMng.Database.Client
import           IgMng.IgRouter
import qualified Options.Applicative       as OA
import           System.Exit               (exitFailure)
import           System.Log.Logger         (errorM, infoM, warningM)

data Cmd = CmdCheck

data Opts = Opts
    { optNoUpdate    :: !Bool
    , optEnvFilePath :: String
    , optCmd         :: Cmd
    }

checkCmd :: OA.Mod OA.CommandFields Cmd
checkCmd = OA.command "check"
    $ OA.info  (pure CmdCheck)
    $ OA.progDesc "check latest diff"

noUpdate :: OA.Parser Bool
noUpdate = OA.switch $ mconcat [
    OA.long "no-update"
  , OA.short 'n'
  , OA.help "Does not update follower status"
  ]

envFilePath :: OA.Parser String
envFilePath = OA.option OA.str $ mconcat [
    OA.long "env-file-path"
  , OA.short 'f'
  , OA.value "./containers/.env"
  , OA.help "The .env file path"
  , OA.metavar "<filepath>"
  ]

programOptions :: OA.Parser Opts
programOptions = Opts
    <$> noUpdate
    <*> envFilePath
    <*> OA.hsubparser (mconcat [
        checkCmd
      ])

optsParser :: OA.ParserInfo Opts
optsParser = OA.info (OA.helper <*> programOptions) $ mconcat [
    OA.fullDesc
  , OA.progDesc "instagram followers logger"
  ]

registerLog :: MySQLConn -> IO Bool
registerLog conn = isJust <$> runMaybeT registerLog'
    where
        registerLog' = do
            rqRes <- MaybeT $ requestFollowers "localhost" 3000
            gotTime <- lift (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)
            oar <- lift (okAffectedRows <$> insertLog conn gotTime rqRes)
            when (oar /= 1) $
                lift (errorM "igmng.registerLog" "failed to register followers") >> mzero

main' :: Opts -> MySQLConn -> IO ()
main' opts conn = case optCmd opts of
    CmdCheck
        | optNoUpdate opts -> selectLatestDiffLog conn >>= \case
            Nothing -> warningM "igmng.main'" "There is no comparison target"
            Just latest -> if uncurry (==) latest then infoM "igmng.main'" "no update" else
                infoM "igmng.main'" "update found"
        | otherwise -> whenM (registerLog conn) $ main' (opts { optNoUpdate = True }) conn

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    newClient (optEnvFilePath opts)
        >>= maybe exitFailure pure
        >>= main' opts
