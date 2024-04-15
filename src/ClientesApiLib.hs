{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ClientesApiLib where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import TransacoesVMsLib (InputTransacao, Saldo)
import ExtratoVMsLib (ExtratoNaHora)

type ClientesApi = "clientes" :> Capture "id" Integer :> "transacoes"
                         :> ReqBody '[JSON] InputTransacao
                         :> Post '[JSON] Saldo
              :<|> "clientes" :> Capture "id" Integer :> "extrato"
                         :> Get '[JSON] ExtratoNaHora

clientesServidor :: Server ClientesApi
clientesServidor = (\a b -> return "Transacoes dummy")
              :<|> (\a   -> return "Extrato dummy"   )

