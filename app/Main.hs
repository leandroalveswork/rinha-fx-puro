module Main where

import Servant          (serve)
import ClientesApiLib (ClientesApi, clientesServidor)

proxyServidor :: Proxy ClientesApi
proxyServidor = Proxy

appRinha :: Application
appRinha = serve proxyServidor clientesServidor

main :: IO ()
main = run 9999 appRinha
