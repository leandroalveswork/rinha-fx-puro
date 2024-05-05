module Main where

import Data.ByteString             (ByteString)
import Data.Maybe                  (fromMaybe)
import Text.Read                   (readMaybe)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Pool
import Database.PostgreSQL.Simple
import Configuration.Dotenv        (parseFile)
import Clientes.ApiLib.Api         (ClientesApi, clientesServidor)
import RepositoryLib.Repository    (migrateDB, initConnectionPool, DBConnectionString, dotenvConnstr)

proxyServidor :: Proxy ClientesApi
proxyServidor = Proxy

appRinha :: Pool Connection -> Application
appRinha conns = serve proxyServidor (clientesServidor conns)

dotenvExposedport :: IO Int
dotenvExposedport = do
  vars <- parseFile ".env"
  let port = fromMaybe "80" (lookup "RINHAFX_PORT" vars)
  let validatedPort = readMaybe port
  case validatedPort of
    Nothing        -> fail "Falha ao ler porta configurada"
    Just validPort -> return validPort

main :: IO ()
main = do
  connstr <- dotenvConnstr
  exposedport <- dotenvExposedport
  pool <- initConnectionPool connstr
  migrateDB connstr
  run exposedport (serve proxyServidor $ clientesServidor pool)
