module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Clientes.ApiLib.Api           (ClientesApi, clientesServidor)
import RepositoryLib.Repository      (migrateDB, initConnectionPool)

proxyServidor :: Proxy ClientesApi
proxyServidor = Proxy

appRinha :: Application
appRinha = serve proxyServidor clientesServidor

main :: IO ()
main = do
  pool <- initConnectionPool "Host=localhost;Port=5432;Database=rinhafx;User ID=root;Password=1234"
  migrateDB pool
  run 9999 (serve proxyServidor $ clientesServidor pool)
