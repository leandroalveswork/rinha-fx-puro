{-# LANGUAGE DeriveGeneric #-}

module Clientes.ViewModelsLib.SaldoNaHoraVM
  ( SaldoNaHora(SaldoNaHora)
  , total
  , limite
  , data_extrato
  ) where

import Data.Time
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics


data SaldoNaHora = SaldoNaHora
  { total        :: Int
  , limite       :: Int
  , data_extrato :: UTCTime
  } deriving Generic

instance ToJSON SaldoNaHora

