module IgMng.Database.Type (
    MySQLType (..)
) where

import qualified Data.Text           as T
import           Data.Time           (LocalTime)
import           Data.Word
import           Database.MySQL.Base

class MySQLType a where
    toMySQLV :: a -> MySQLValue

instance MySQLType T.Text where
    toMySQLV = MySQLText

instance MySQLType Word64 where
    toMySQLV = MySQLInt64U

instance MySQLType LocalTime where
    toMySQLV = MySQLDateTime
