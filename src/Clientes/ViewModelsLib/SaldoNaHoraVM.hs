{-# LANGUAGE DeriveGeneric #-}

module Clientes.ViewModelsLib.SaldoNaHoraVM
  ( SaldoNaHora
  ) where

import Data.Time
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics


data SaldoNaHora = SaldoNaHora
  { total        :: Integer
  , limite       :: Integer
  , data_extrato :: UTCTime
  } deriving Generic

instance ToJSON SaldoNaHora

