{-# LANGUAGE ExplicitNamespaces, LambdaCase, OverloadedStrings, Rank2Types,
             TypeOperators #-}
module Main where

import           Control.Monad             (MonadPlus (..), unless, when)
import           Control.Monad.Extra       (unlessM, whenM)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Algorithm.Diff       (Diff (..), getDiffBy)
import           Data.Maybe                (fromJust, isJust)
import qualified Data.Text.IO              as T
import           Data.Tuple.Extra          (both)
import           Data.Word
import           Database.MySQL.Base       (MySQLConn, OK (..))
import           IgMng.Database.Client
import           IgMng.IgRouter
import           IgMng.IO                  (putStrLnErr)
import           IgMng.LineNotify
import           IgMng.Utils
import           Network.Socket            (HostName)
import qualified Options.Applicative       as OA
import           System.Directory          (getHomeDirectory)
import           System.Exit               (exitFailure)
import           System.FilePath           ((</>))
import           Text.Read                 (readMaybe)

data Cmd = CmdCheck
    | CmdFetch
    | CmdDelete

data Opts = Opts
    { optNoFetch               :: !Bool
    , optEnableLineNotify      :: !Bool
    , optEnvFilePath           :: FilePath
    , optCredentialsFilePath   :: FilePath
    , optLimitNum              :: Word64
    , optIgMngRouterHostName   :: HostName
    , optIgMngDatabaseHostName :: HostName
    , optYearAgo               :: Maybe Word64
    , optCmd                   :: Cmd
    }

checkCmd :: OA.Mod OA.CommandFields Cmd
checkCmd = OA.command "check"
    $ OA.info  (pure CmdCheck)
    $ OA.progDesc "fetch and check latest diff"

fetchCmd :: OA.Mod OA.CommandFields Cmd
fetchCmd = OA.command "fetch"
    $ OA.info (pure CmdFetch)
    $ OA.progDesc "fetch followers status"

deleteCmd :: OA.Mod OA.CommandFields Cmd
deleteCmd = OA.command "delete"
    $ OA.info (pure CmdDelete)
    $ OA.progDesc "delete logs"

noFetch :: OA.Parser Bool
noFetch = OA.switch $ mconcat [
    OA.long "no-fetch"
  , OA.short 'n'
  , OA.help "Does not fetch followers status"
  ]

enableLine :: OA.Parser Bool
enableLine = OA.switch $ mconcat [
    OA.long "enable-line-notify"
  , OA.help "enable line notify"
  ]

envFilePath :: OA.Parser FilePath
envFilePath = OA.option OA.str $ mconcat [
    OA.long "env-file-path"
  , OA.value "./containers/.env"
  , OA.help "The .env file path"
  , OA.metavar "<filepath>"
  ]

credentialsFilePath :: FilePath -> OA.Parser FilePath
credentialsFilePath homeDir = OA.option OA.str $ mconcat [
    OA.long "credentials-file-path"
  , OA.value $ homeDir </> ".igmng" </> "credentials.toml"
  , OA.help "credentials.toml file"
  , OA.metavar "<filepath>"
  ]

limitNum :: OA.Parser Word64
limitNum = OA.option OA.auto $ mconcat [
    OA.long "limit"
  , OA.value 0
  , OA.help "the number of deleting"
  , OA.metavar "<delete log number>"
  ]

igMngRouterHostName :: OA.Parser HostName
igMngRouterHostName = OA.option OA.str $ mconcat [
    OA.long "router-hostname"
  , OA.value "localhost"
  , OA.metavar "<hostname>"
  ]

igMngDatabaseHostName :: OA.Parser HostName
igMngDatabaseHostName = OA.option OA.str $ mconcat [
    OA.long "db-hostname"
  , OA.value "localhost"
  , OA.metavar "<hostname>"
  ]

yearAgo :: OA.Parser (Maybe Word64)
yearAgo = OA.option (OA.maybeReader (Just . (readMaybe :: String -> Maybe Word64))) $ mconcat [
    OA.long "year-ago"
  , OA.value Nothing
  , OA.help "the number of delete year"
  , OA.metavar "yyyy"
  ]

programOptions :: FilePath -> OA.Parser Opts
programOptions homeDir = Opts
    <$> noFetch
    <*> enableLine
    <*> envFilePath
    <*> credentialsFilePath homeDir
    <*> limitNum
    <*> igMngRouterHostName
    <*> igMngDatabaseHostName
    <*> yearAgo
    <*> OA.hsubparser (mconcat [
        checkCmd
      , fetchCmd
      , deleteCmd
      ])

optsParser :: FilePath -> OA.ParserInfo Opts
optsParser homeDir = OA.info (OA.helper <*> programOptions homeDir) $ mconcat [
    OA.fullDesc
  , OA.progDesc "instagram followers logger"
  ]

registerLog :: MySQLConn -> String -> IO Bool
registerLog conn host = isJust <$> runMaybeT registerLog'
    where
        registerLog' = do
            rqRes <- MaybeT $ requestFollowers host 3000
            gotTime <- lift getCurrentLocalTime
            oar <- lift (okAffectedRows <$> insertLog conn gotTime rqRes)
            when (oar /= 1) $
                lift (putStrLnErr "igmng.registerLog: failed to register followers") >> mzero

main' :: Opts -> MySQLConn -> IO ()
main' opts conn = case optCmd opts of
    CmdCheck
        | optNoFetch opts -> selectLatestDiffLog conn >>= \case
            Nothing -> putStrLn "igmng.main': There is no comparison target"
            Just latest ->
                let onlySecond = map (name . fromSecond)
                        $ filter isSecond
                        $ uncurry (getDiffBy (flip (.) userId . (==) . userId))
                        $ both followers latest
                in if null onlySecond then putStrLn "igmng.main': no unfollow" else
                    putStrLn "igmng.main': unfollow found"
                        >> mapM_ T.putStrLn onlySecond
                        >> when (optEnableLineNotify opts)
                            (unlessM (notifyLine (optCredentialsFilePath opts) onlySecond) exitFailure)
        | otherwise -> whenM (registerLog conn $ optIgMngRouterHostName opts) $ main' (opts { optNoFetch = True }) conn
    CmdFetch -> unless (optNoFetch opts)
        $ whenM (registerLog conn $ optIgMngRouterHostName opts)
        $ putStrLn "igmng.main': fetch complete"
    CmdDelete
        | isJust (optYearAgo opts) -> currentTimeYearAgo (fromJust $ optYearAgo opts)
            >>= deleteLogWithYearAgo conn (optLimitNum opts)
            >>= putStrLn . mappend "igmng.main': " . show
        | otherwise -> deleteLog conn (optLimitNum opts)
            >>= putStrLn . mappend "igmng.main': " . show

main :: IO ()
main = do
    opts <- OA.execParser . optsParser =<< getHomeDirectory
    newClient (optEnvFilePath opts) (optIgMngDatabaseHostName opts)
        >>= maybe exitFailure pure
        >>= main' opts
