{-# LANGUAGE ExplicitNamespaces, LambdaCase, OverloadedStrings, Rank2Types,
             TypeOperators #-}
module Main where

import           Control.Monad             (MonadPlus (..), unless, when)
import           Control.Monad.Extra       (whenM)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Control.Natural           (type (~>))
import           Data.Algorithm.Diff       (Diff (..), getDiffBy)
import           Data.List                 (partition)
import           Data.Maybe                (fromJust, isJust)
import qualified Data.Text.IO              as T
import           Data.Time                 (LocalTime (..), TimeOfDay (..),
                                            UTCTime (..), getCurrentTime,
                                            getCurrentTimeZone, utcToLocalTime)
import           Data.Time.Calendar        (addDays)
import           Data.Tuple.Extra          (both)
import           Data.Word
import           Database.MySQL.Base       (MySQLConn, OK (..))
import           IgMng.Database.Client
import           IgMng.IgRouter
import           IgMng.IO                  (putStrLnErr)
import           IgMng.LineNotify
import qualified Options.Applicative       as OA
import           System.Exit               (exitFailure)
import           Text.Read                 (readMaybe)

data Cmd = CmdCheck
    | CmdFetch
    | CmdDelete

data Opts = Opts
    { optNoFetch          :: !Bool
    , optEnableLineNotify :: !Bool
    , optEnvFilePath      :: String
    , optLimitNum         :: Word64
    , optYearAgo          :: Maybe Word64
    , optCmd              :: Cmd
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

envFilePath :: OA.Parser String
envFilePath = OA.option OA.str $ mconcat [
    OA.long "env-file-path"
  , OA.short 'f'
  , OA.value "./containers/.env"
  , OA.help "The .env file path"
  , OA.metavar "<filepath>"
  ]

limitNum :: OA.Parser Word64
limitNum = OA.option OA.auto $ mconcat [
    OA.long "limit"
  , OA.value 0
  , OA.help "the number of deleting"
  , OA.metavar "<delete log number>"
  ]

yearAgo :: OA.Parser (Maybe Word64)
yearAgo = OA.option (OA.maybeReader (Just . (readMaybe :: String -> Maybe Word64))) $ mconcat [
    OA.long "year-ago"
  , OA.value Nothing
  , OA.help "the number of delete year"
  , OA.metavar "yyyy"
  ]

programOptions :: OA.Parser Opts
programOptions = Opts
    <$> noFetch
    <*> enableLine
    <*> envFilePath
    <*> limitNum
    <*> yearAgo
    <*> OA.hsubparser (mconcat [
        checkCmd
      , fetchCmd
      , deleteCmd
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
                lift (putStrLnErr "igmng.registerLog: failed to register followers") >> mzero

currentTimeYearAgo :: Word64 -> IO LocalTime
currentTimeYearAgo y = do
    gotTime <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
    pure gotTime {
          localDay = addDays (negate (fromIntegral (365 * y) :: Integer)) (localDay gotTime)
        , localTimeOfDay = (localTimeOfDay gotTime) { todSec = 0 }
        }

isSecond :: Diff a -> Bool
isSecond (Second _) = True
isSecond _          = False

isBoth :: Diff a -> Bool
isBoth (Both _ _) = True
isBoth _          = False

fromSecond :: Diff a -> a
fromSecond (Second x) = x
fromSecond _          = error "fromSecond: not second"

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
                        >> when (optEnableLineNotify opts) (notifyLine onlySecond)
        | otherwise -> whenM (registerLog conn) $ main' (opts { optNoFetch = True }) conn
    CmdFetch -> unless (optNoFetch opts) $ whenM (registerLog conn) $ putStrLn "igmng.main': fetch complete"
    CmdDelete
        | isJust (optYearAgo opts) -> currentTimeYearAgo (fromJust $ optYearAgo opts)
            >>= deleteLogWithYearAgo conn (optLimitNum opts)
            >>= putStrLn . mappend "igmng.main': " . show
        | otherwise -> deleteLog conn (optLimitNum opts)
            >>= putStrLn . mappend "igmng.main': " . show

main :: IO ()
main = do
    opts <- OA.execParser optsParser
    newClient (optEnvFilePath opts)
        >>= maybe exitFailure pure
        >>= main' opts
