module Clientes.ViewModelsLib.TipoTransacaoVM
  ( TipoTransacao
  , sinalDe
  , unshowPerigoso
  ) where

import Data.Time
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics


data TipoTransacao = Credito | Debito

instance FromJSON TipoTransacao where
  parseJSON = withText "TipoTransacao" $ \v ->
    case unpack v of
      "c" -> return Credito
      "d" -> return Debito
      _   -> fail "Não foi possivel"

instance ToJSON TipoTransacao where
  toJSON Credito = String (pack "c")
  toJSON Debito  = String (pack "d")

instance Show TipoTransacao where
  show Credito = "c"
  show Debito  = "d"

unshowPerigoso :: String -> TipoTransacao
unshowPerigoso "c" = Credito
unshowPerigoso "d" = Debito
unshowPerigoso _   = Debito

sinalDe :: TipoTransacao -> Int
sinalDe Credito = 1
sinalDe Debito  = -1
