{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.Environment (getArgs, lookupEnv)
import System.IO (appendFile, stdout, hPutStrLn)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Aeson (ToJSON, encode, toJSON, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (toLower)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Define the Log Level
data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON LogLevel where
  toJSON = toJSON . map toLower . show

-- Define the structured Log Message
data LogMessage = LogMessage
  { level     :: LogLevel
  , timestamp :: String
  , message   :: String
  , context   :: Map String String
  } deriving (Show, Generic)

instance ToJSON LogMessage

-- Get current timestamp
getCurrentTimestamp :: IO String
getCurrentTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime

-- Create a log message
createLogMessage :: LogLevel -> String -> Map String String -> IO LogMessage
createLogMessage lvl msg ctx = do
  ts <- getCurrentTimestamp
  return $ LogMessage lvl ts msg ctx

-- Write to file
writeJsonLog :: FilePath -> LogMessage -> IO ()
writeJsonLog path logMsg = appendFile path (B.unpack (encode logMsg) ++ "\n")

-- Write to stdout
writeStdoutJsonLog :: LogMessage -> IO ()
writeStdoutJsonLog logMsg = hPutStrLn stdout (B.unpack $ encode logMsg)

-- Parse log level safely
parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = case map toLower s of
  "debug"   -> Just Debug
  "info"    -> Just Info
  "warning" -> Just Warning
  "error"   -> Just Error
  _         -> Nothing

-- Build context from environment
getContextFromEnv :: IO (Map String String)
getContextFromEnv = do
  maybeUser <- lookupEnv "USER"
  maybeApp  <- lookupEnv "APP_ENV"
  return $ Map.fromList
    [ ("user", fromMaybe "unknown" maybeUser)
    , ("env",  fromMaybe "development" maybeApp)
    ]

-- Main logic
main :: IO ()
main = do
  args <- getArgs
  case args of
    [logFile, levelStr, msg] -> case parseLogLevel levelStr of
      Just level -> do
        ctx <- getContextFromEnv
        logMsg <- createLogMessage level msg ctx
        writeJsonLog logFile logMsg
        writeStdoutJsonLog logMsg
      Nothing -> putStrLn "Invalid log level. Use: debug | info | warning | error"
    _ -> putStrLn "Usage: structuredlogapp <log_file> <level> <message>"
