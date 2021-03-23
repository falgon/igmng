module IgMng.Utils (
    getCurrentLocalTime
  , currentTimeYearAgo
  , isSecond
  , fromSecond
) where

import           Data.Algorithm.Diff (Diff (..))
import           Data.Time           (LocalTime (..), TimeOfDay (..),
                                      getCurrentTime, getCurrentTimeZone,
                                      utcToLocalTime)
import           Data.Time.Calendar  (addDays)
import           Data.Word           (Word64)

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

currentTimeYearAgo :: Word64 -> IO LocalTime
currentTimeYearAgo y = do
    gotTime <- getCurrentLocalTime
    pure gotTime {
          localDay = addDays (negate (fromIntegral (365 * y) :: Integer)) (localDay gotTime)
        , localTimeOfDay = (localTimeOfDay gotTime) { todSec = 0 }
        }

isSecond :: Diff a -> Bool
isSecond (Second _) = True
isSecond _          = False

fromSecond :: Diff a -> a
fromSecond (Second x) = x
fromSecond _          = error "fromSecond: not second"
