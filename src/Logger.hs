{-# LANGUAGE OverloadedStrings #-}
module Logger
  ( adapt
  , defaultLogEnv
  , logMsg
  , runKatipT
  , KatipT(..)
  , Katip(..)
  , LogEnv
  , Severity(..)
  )where


import           Control.Monad.Logger
import qualified Control.Monad.Logger   as Logger
import           Katip
import qualified System.IO              as IO
import qualified System.Log.FastLogger  as FastLogger





defaultLogEnv :: IO LogEnv
defaultLogEnv = do
  handelScribe <- mkHandleScribe ColorIfTerminal IO.stdout DebugS V2
  env <- initLogEnv "servant-persistent" "production"
  registerScribe "stdout" handelScribe defaultScribeSettings env

fromLogLevel :: LogLevel -> Severity
fromLogLevel LevelDebug = DebugS
fromLogLevel LevelInfo = InfoS
fromLogLevel LevelWarn = WarningS
fromLogLevel LevelError = ErrorS
fromLogLevel (LevelOther _) = NoticeS



adapt :: (ToLogStr msg, Applicative m, Katip m) =>
         (Namespace -> Severity -> Katip.LogStr -> m()) ->
         Loc -> LogSource -> LogLevel -> msg -> m()
adapt f _ src lvl msg =
  f ns (fromLogLevel lvl) $ logStr' msg
    where
      ns = Namespace [src]
      logStr' = Katip.logStr . FastLogger.fromLogStr . Logger.toLogStr
      





