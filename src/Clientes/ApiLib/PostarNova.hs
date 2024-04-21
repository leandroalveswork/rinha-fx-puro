{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Clientes.ApiLib.PostarNova
  ( postarNova
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Servant
import Clientes.ViewModelsLib.InputTransacaoVM (InputTransacao, valor, tipo, descricao)
import Clientes.ViewModelsLib.SaldoVM          (Saldo, Saldo(Saldo), saldo, limite)
import Clientes.ViewModelsLib.TipoTransacaoVM  (TipoTransacao, sinalDe)

postarNova :: Pool Connection -> Int -> InputTransacao -> Handler Saldo
postarNova conns idCliente inptTr = do
  saldos <- liftIO $
    withResource conns $ \conn ->
      (query conn
        "exec SP_NOVA_TRANSACAO ? ? ? ?"
        ( idCliente                              :: Int
        , (valor inptTr * (sinalDe.tipo) inptTr) :: Int
        , (pack.show.tipo) inptTr                :: Text
        , (pack.descricao) inptTr                :: Text
        ) :: IO [(Int, Int)])
  case saldos of
    []         -> throwError err404
    (xsaldo:_) -> return Saldo { saldo = fst xsaldo , limite = snd xsaldo }

