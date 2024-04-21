{-# LANGUAGE DeriveGeneric #-}

module Clientes.ViewModelsLib.SaldoVM
  ( Saldo(Saldo)
  , saldo
  , limite
  ) where

import Data.Time
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Database.PostgreSQL.Simple


data Saldo = Saldo
  { saldo  :: Int
  , limite :: Int
  } deriving Generic

instance ToJSON Saldo

