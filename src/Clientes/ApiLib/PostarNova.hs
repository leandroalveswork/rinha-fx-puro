{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Clientes.ApiLib.PostarNova
  ( postarNova
  ) where

import Data.ByteString (ByteString)
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Clientes.ViewModelsLib.InputTransacaoVM (InputTransacao, valor, tipo, descricao)
import Clientes.ViewModelsLib.SaldoVM          (Saldo)

type DBConnectionString = ByteString

postarNova :: Integer -> InputTransacao -> Handler Saldo
postarNova idCliente inptTr = fmap (map fromOnly) . liftIO $
                      withResource conns $ \conn ->
                        query conn
                          "exec SP_NOVA_TRANSACAO ? ? ? ?"
                          ( idCliente
                          , valor inptTr 
                          , show.tipo inptTr
                          , descricao inptTr
                          )

