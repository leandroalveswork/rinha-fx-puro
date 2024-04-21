{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Clientes.ApiLib.Api
  ( ClientesApi
  , clientesServidor
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.String.Conversions
import GHC.Generics
import Data.Pool
import Database.PostgreSQL.Simple
import Servant
import Clientes.ViewModelsLib.ExtratoNaHoraVM  (ExtratoNaHora)
import Clientes.ViewModelsLib.InputTransacaoVM (InputTransacao)
import Clientes.ViewModelsLib.SaldoVM          (Saldo)
import Clientes.ApiLib.PostarNova              (postarNova) 
import Clientes.ApiLib.ObterExtrato            (obterExtrato) 

type ClientesApi = "clientes" :> Capture "id" Int :> "transacoes"
                         :> ReqBody '[JSON] InputTransacao
                         :> Post '[JSON] Saldo
              :<|> "clientes" :> Capture "id" Int :> "extrato"
                         :> Get '[JSON] ExtratoNaHora

clientesServidor :: Pool Connection -> Server ClientesApi
clientesServidor conns = postarNova conns
                    :<|> obterExtrato conns


