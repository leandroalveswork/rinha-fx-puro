{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Clientes.ApiLib.ObterExtrato
  ( obterExtrato
  ) where

import Data.ByteString                          (ByteString)
import Data.Text                                (Text, unpack)
import Control.Exception                        (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Servant
import Data.Time                                (UTCTime)
import Clientes.ViewModelsLib.TipoTransacaoVM   (unshowPerigoso)
import Clientes.ViewModelsLib.InputTransacaoVM  (InputTransacao)
import Clientes.ViewModelsLib.SaldoVM           (Saldo)
import Clientes.ViewModelsLib.SaldoNaHoraVM     (SaldoNaHora(SaldoNaHora), total, limite, data_extrato)
import Clientes.ViewModelsLib.ExtratoNaHoraVM   (ExtratoNaHora, ExtratoNaHora (ExtratoNaHora), saldo, ultimas_transacoes)
import Clientes.ViewModelsLib.TransacaoNaHoraVM (TransacaoNaHora (TransacaoNaHora), valor, tipo, descricao, realizada_em)

type DBConnectionString = ByteString

fromRowTransacao :: (Int, Text, Text, UTCTime) -> TransacaoNaHora
fromRowTransacao (xvalor, xtipo, xdescricao, xrealizadaEm) =
  TransacaoNaHora { valor        = xvalor
                  , tipo         = (unshowPerigoso.unpack) xtipo
                  , descricao    = unpack xdescricao
                  , realizada_em = xrealizadaEm
                  } 

fromRowSaldo :: (Int, Int, UTCTime) -> SaldoNaHora
fromRowSaldo (xsaldo, xlimite, xdataExtrato) =
  SaldoNaHora { total = xsaldo
              , limite = xlimite
              , data_extrato = xdataExtrato
              }

obterExtrato :: Pool Connection -> Int -> Handler ExtratoNaHora
obterExtrato conns idCliente = do
  transacoes <- fmap (map fromRowTransacao) . liftIO $
    withResource conns $ \conn ->
      (query conn
        (  "SELECT valor, tipo, descricao, realizada_em"
        <> " FROM Transacoes WHERE id_cliente = ?"
        <> " ORDER BY realizada_em DESC"
        <> " LIMIT 10;"
        )
        ( Only idCliente :: Only Int )
        :: IO [(Int, Text, Text, UTCTime)])

  saldos <- fmap (map fromRowSaldo) . liftIO $
    withResource conns $ \conn ->
      (query conn
        (  "SELECT saldo, limite, NOW()"
        <> " FROM Clientes WHERE id = ?;"
        )
        ( Only idCliente :: Only Int )
        :: IO [(Int, Int, UTCTime)])

  case saldos of
    []         -> throwError err404
    (xsaldo:_) -> return ExtratoNaHora { saldo = xsaldo , ultimas_transacoes = transacoes }

