module Main where

import Data.ByteString             (ByteString)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Pool
import Database.PostgreSQL.Simple
import Clientes.ApiLib.Api         (ClientesApi, clientesServidor)
import RepositoryLib.Repository    (migrateDB, initConnectionPool, DBConnectionString, dotenvConnstr)
import Data.String (IsString(fromString))

proxyServidor :: Proxy ClientesApi
proxyServidor = Proxy

appRinha :: Pool Connection -> Application
appRinha conns = serve proxyServidor (clientesServidor conns)

main :: IO ()
main = do
  connstr <- dotenvConnstr
  pool <- initConnectionPool connstr
  migrateDB connstr
  run 9999 (serve proxyServidor $ clientesServidor pool)
