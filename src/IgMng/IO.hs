module IgMng.IO (
    putStrLnErr
) where

import           System.IO (hPutStrLn, stderr)

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr
