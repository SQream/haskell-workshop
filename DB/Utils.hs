
module DB.Utils
  ( ppTable
  , checkTable
  , getName
  , trim
  , compose
  , isJust
  , fromJust
  --, runWithError
  --, runServerLoop
  , lookupCol

  , intercalate  -- from Data.List
  , catch
  , SomeException(..)
  , readQuery
  , repl
  --, recv
  --, send
  )
where

import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Text.Read (readMaybe)
import Control.Monad (forever)
import Control.Exception
--import Network.Socket
--import Control.Concurrent (forkIO)
import System.IO

getName = fst

getSchema table = case table of
  [] -> []
  (row:_) -> map fst row


-- 3 -- pretty print a table

ppCol col = show (snd col)

ppRow row = unwords (map ppCol row)

ppTable table = unlines (map ppRow table)

readQuery :: Read a => String -> a
readQuery = fromMaybe (error "Could not understand query.") . readMaybe

repl :: (IO ()) -> IO ()
repl io = do
  hSetBuffering stdout NoBuffering
  forever $ do
    putStr "> "
    catch io (\(SomeException e) -> hPutStrLn stderr (show e ++ "\n"))

checkRow schema row =
  let
    colNames = map fst row
  in sort colNames == nub colNames
     && length colNames == length schema
     && all id (zipWith (==) colNames schema)

checkTable table =
  all (checkRow (getSchema table)) table

trim text = unwords (words text)

compose = (.)

lookupCol colName row = case lookup colName row of
  Nothing -> error ("Could not find column " ++ colName)
  Just val -> val

{-
runWithError action =
  action `catch` \(SomeException e) -> print e

runServerLoop :: Int -> (Socket -> IO ()) -> IO ()
runServerLoop port handleSocket = do
  addrInfos <- getAddrInfo Nothing Nothing (Just (show port))
  let serverAddr = head addrInfos
  bracket
    (socket (addrFamily serverAddr) Stream defaultProtocol)
    close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress serverAddr)
      listen sock 1
      putStrLn "listening..."
      forever $ do
        (soc, _) <- accept sock
        forkIO $ do
          -- we could also let the student supply this handler
          putStrLn "accepted socket!"
          handleSocket soc
          close soc
          putStrLn "closed socket!"

-}
