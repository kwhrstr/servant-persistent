{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import           Control.Monad.Reader (runReaderT)
import           Servant              ((:<|>) ((:<|>)),
                                       Proxy (Proxy), Raw, Server,
                                       serve, serveDirectoryFileServer)
import           Servant.Server

import           Api.User             (UserAPI, userServer, userApi)
import           Config               (AppT (..), Config (..), App)

userApp :: Config -> Application
userApp cfg = serve userApi $ appToServer cfg

appToServer :: Config -> Server UserAPI
appToServer cfg = hoistServer userApi (convertApp cfg) userServer

convertApp :: Config -> App a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

files :: Server Raw
files = serveDirectoryFileServer "assets"


type AppAPI = UserAPI :<|> Raw

appApi :: Proxy AppAPI
appApi = Proxy


app :: Config -> Application
app cfg = serve appApi (appToServer cfg :<|> files)