{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Clientes.ApiLib.Api
  ( ClientesApi
  , clientesServidor
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString                         (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Clientes.ViewModelsLib.ExtratoNaHoraVM  (ExtratoNaHora)
import Clientes.ViewModelsLib.InputTransacaoVM (InputTransacao)
import Clientes.ViewModelsLib.SaldoVM          (Saldo)
import Clientes.ApiLib.PostarNova              (postarNova) 
import Clientes.ApiLib.ObterExtrato            (obterExtrato) 

type ClientesApi = "clientes" :> Capture "id" Integer :> "transacoes"
                         :> ReqBody '[JSON] InputTransacao
                         :> Post '[JSON] Saldo
              :<|> "clientes" :> Capture "id" Integer :> "extrato"
                         :> Get '[JSON] ExtratoNaHora

clientesServidor :: Server ClientesApi
clientesServidor = postarNova
              :<|> obterExtrato


