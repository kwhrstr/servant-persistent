{-# LANGUAGE OverloadedStrings #-}

module Init where

import           Control.Concurrent          (killThread)
import qualified Control.Monad.Metrics       as M
import           Database.Persist.Postgresql (runSqlPool)
import           Lens.Micro                  ((^.))
import           Network.Wai                 (Application)
import           Network.Wai.Metrics         (metrics, registerWaiMetrics)
import           System.Environment          (lookupEnv)
import           System.Remote.Monitoring    (forkServer, serverMetricStore,
                                              serverThreadId)

import           Api                         (app)
import           Api.User                    (generateJavaScript)
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger)
import           Control.Exception           (bracket)
import qualified Data.Pool                   as Pool
import qualified Katip
import           Logger                      (defaultLogEnv)
import           Models                      (doMigrations)
import           Network.Wai.Handler.Warp    (run)
import           Safe                        (readMay)


runApp :: IO()
runApp = bracket acquireConfig shutdownApp runApp'
  where
    runApp' config = run (configPort config) =<< initialize config

initialize :: Config -> IO Application
initialize cfg = do
  waiMetrics <- registerWaiMetrics (configMetrics cfg ^. M.metricsStore)
  let logger = setLogger (configEnv cfg)
  runSqlPool doMigrations (configPool cfg)
  generateJavaScript
  pure . logger . metrics waiMetrics . app $ cfg

acquireConfig :: IO Config
acquireConfig = do
  port <- lookupSetting "PORT" 8081
  env <- lookupSetting "ENV" Development
  logEnv <- defaultLogEnv
  pool <- makePool env logEnv
  ekgServer <- forkServer "localhost" 8000
  let store = serverMetricStore ekgServer
  waiMetrics <- registerWaiMetrics store
  metr <- M.initializeWith store
  pure Config
        { configPool = pool
        , configEnv = env
        , configMetrics = metr
        , configLogEnv = logEnv
        , configPort = port
        , configEkgServer = serverThreadId ekgServer
        }


shutdownApp :: Config -> IO ()
shutdownApp cfg = do
  Katip.closeScribes (configLogEnv cfg)
  Pool.destroyAllResources (configPool cfg)
  killThread (configEkgServer cfg)
  pure ()

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> return def
    Just str ->
      maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $ mconcat
        [ "Failet to read [["
        , str
        , "]] for environment variable "
        , env
        ]



