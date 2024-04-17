{-# LANGUAGE DeriveGeneric #-}

module Clientes.ViewModelsLib.TransacaoNaHoraVM
  ( TransacaoNaHora
  ) where

import Data.Time
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Clientes.ViewModelsLib.TipoTransacaoVM (TipoTransacao)


data TransacaoNaHora = TransacaoNaHora
  { valor        :: Integer
  , tipo         :: TipoTransacao
  , descricao    :: String
  , realizada_em :: UTCTime
  } deriving Generic

instance ToJSON TransacaoNaHora

