{-# LANGUAGE DeriveGeneric #-}

module Clientes.ViewModelsLib.SaldoVM
  ( Saldo
  ) where

import Data.Time
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics


data Saldo = Saldo
  { saldo  :: Integer
  , limite :: Integer
  } deriving Generic

instance ToJSON Saldo

