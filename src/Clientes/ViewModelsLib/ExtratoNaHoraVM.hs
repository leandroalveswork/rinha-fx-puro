{-# LANGUAGE DeriveGeneric #-}

module Clientes.ViewModelsLib.ExtratoNaHoraVM
  ( ExtratoNaHora (ExtratoNaHora)
  , saldo
  , ultimas_transacoes
  ) where

import Data.Time
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Clientes.ViewModelsLib.SaldoNaHoraVM     (SaldoNaHora)
import Clientes.ViewModelsLib.TransacaoNaHoraVM (TransacaoNaHora)


data ExtratoNaHora = ExtratoNaHora
  { saldo              :: SaldoNaHora
  , ultimas_transacoes :: [TransacaoNaHora]
  } deriving Generic

instance ToJSON ExtratoNaHora

