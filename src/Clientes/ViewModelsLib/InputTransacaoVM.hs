{-# LANGUAGE DeriveGeneric #-}

module Clientes.ViewModelsLib.InputTransacaoVM
  ( InputTransacao
  , valor
  , tipo
  , descricao
  ) where

import Data.Time
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Clientes.ViewModelsLib.TipoTransacaoVM (TipoTransacao)


data InputTransacao = InputTransacao
  { valor     :: Integer
  , tipo      :: TipoTransacao
  , descricao :: String
  } deriving Generic

instance FromJSON InputTransacao

