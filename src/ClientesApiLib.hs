module ClientesApiLib where

type ClientesApi = "clientes/:id/transacoes" :> Post '[JSON] SaldoVM
              :<|> "clientes/:id/extrato" :> Get '[JSON] ExtratoVM

