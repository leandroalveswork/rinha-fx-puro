{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Clientes.ApiLib.ObterExtrato
  ( obterExtrato
  ) where

import Data.ByteString                         (ByteString)
import Control.Concurrent
import Control.Exception                       (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Network.HTTP.Client                     (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Data.Time                               (UTCTime)
import Clientes.ViewModelsLib.InputTransacaoVM (InputTransacao)
import Clientes.ViewModelsLib.SaldoVM          (Saldo)
import Clientes.ViewModelsLib.ExtratoNaHoraVM  (ExtratoNaHora)

type DBConnectionString = ByteString

data TransacaoComSaldo = TransacaoComSaldo
  { valor        :: Integer
  , tipo         :: String
  , descricao    :: String
  , realizada_em :: UTCTime
  , saldo        :: Integer
  }
pegarSaldoPelaTabela :: [TransacaoComSaldo]

obterExtrato :: Integer -> Handler ExtratoNaHora
obterExtrato idCliente = fmap (map fromOnly) . liftIO $
                           withResource conns $ \conn ->
                             query conn
                               "SELECT valor, tipo, descricao, realizada_em, saldo" ++
                               " FROM Transacoes" ++
                               " WHERE id_cliente = ?" ++
                               " ORDER BY realizada_em DESC" ++
                               " LIMIT 10"
                               ( idCliente )

