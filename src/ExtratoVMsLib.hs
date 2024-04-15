{-# LANGUAGE DeriveGeneric #-}

module ExtratoVMsLib where

import ViewModelsLib (TipoTransacao)
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

data TransacaoNaHora = TransacaoNaHora
  { valor        :: Integer
  , tipo         :: TipoTransacao
  , descricao    :: String
  , realizada_em :: UTCTime
  } deriving Generic

instance ToJSON TransacaoNaHora

data ExtratoNaHora = ExtratoNaHora
  { saldo              :: SaldoNaHora
  , ultimas_transacoes :: [TransacaoNaHora]
  } deriving Generic

instance ToJSON ExtratoNaHora

