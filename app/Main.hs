module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Clientes.ApiLib.Api           (ClientesApi, clientesServidor)

proxyServidor :: Proxy ClientesApi
proxyServidor = Proxy

appRinha :: Application
appRinha = serve proxyServidor clientesServidor

main :: IO ()
main = run 9999 appRinha
