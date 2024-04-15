{-# LANGUAGE DeriveGeneric #-}

module ViewModelsLib where

import Data.Time
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data TipoTransacao = Credito | Debito

instance FromJSON TipoTransacao where
  parseJSON = withText $ \v -> case unpack v of
                                 "c" -> Credito
                                 "d" -> Debito
                                 _   -> fail

instance ToJSON TipoTransacao where
  toJSON = \v -> case v of
                   Credito -> pack "c"
                   Debito  -> pack "d"

