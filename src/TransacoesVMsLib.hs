{-# LANGUAGE DeriveGeneric #-}

module TransacoesVMsLib where

import ViewModelsLib (TipoTransacao)
import Data.Time
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data InputTransacao = InputTransacao
  { valor     :: Integer
  , tipo      :: TipoTransacao
  , descricao :: String
  } deriving Generic
  
instance FromJSON InputTransacao

data Saldo = Saldo
  { limite :: Integer
  , saldo  :: Integer
  } deriving Generic

instance ToJSON Saldo

