{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Clientes.ApiLib.ObterExtrato
  ( obterExtrato
  ) where

import Data.ByteString                          (ByteString)
import Control.Concurrent
import Control.Exception                        (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Network.HTTP.Client                      (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Data.Time                                (UTCTime)
import Clientes.ViewModelsLib.InputTransacaoVM  (InputTransacao)
import Clientes.ViewModelsLib.SaldoVM           (Saldo)
import Clientes.ViewModelsLib.SaldoNaHoraVM     (SaldoNaHora)
import Clientes.ViewModelsLib.ExtratoNaHoraVM   (ExtratoNaHora, saldo, ultimas_transacoes)
import Clientes.ViewModelsLib.TransacaoNaHoraVM (TransacaoNaHora)

type DBConnectionString = ByteString

obterExtrato :: Pool Connection -> Integer -> Handler ExtratoNaHora
obterExtrato conns idCliente = do
  transacoes :: [TransacaoNaHora]
  transacoes <- fmap (map fromOnly) . liftIO $
    withResource conns $ \conn ->
      query conn
        "SELECT valor, tipo, descricao, realizada_em" ++
        " FROM Transacoes WHERE id_cliente = ?" ++
        " ORDER BY realizada_em DESC" ++
        " LIMIT 10;"
        ( idCliente )
  saldos :: [SaldoNaHora]
  saldos <- fmap (map fromOnly) . liftIO $
    withResrouce conns $ \conn ->
      query conn
        "SELECT [total] = saldo, limite, data_extrato = NOW()" ++
        " FROM Clientes WHERE id_cliente = ?"
        ( idCliente )
  case saldos of
    []        -> throwError (err404 ())
    [saldo:_] -> return ExtratoNaHora { saldo , ultimas_transacoes = transacoes }

