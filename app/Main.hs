module Main where

import Data.ByteString             (ByteString)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Pool
import Database.PostgreSQL.Simple
import Clientes.ApiLib.Api         (ClientesApi, clientesServidor)
import RepositoryLib.Repository    (migrateDB, initConnectionPool, DBConnectionString)
import Data.String (IsString(fromString))

proxyServidor :: Proxy ClientesApi
proxyServidor = Proxy

appRinha :: Pool Connection -> Application
appRinha conns = serve proxyServidor (clientesServidor conns)

connstr :: DBConnectionString
connstr = fromString "Host=localhost;Port=5432;Database=rinhafx;User ID=root;Password=1234"

main :: IO ()
main = do
  pool <- initConnectionPool connstr
  migrateDB connstr
  run 9999 (serve proxyServidor $ clientesServidor pool)
