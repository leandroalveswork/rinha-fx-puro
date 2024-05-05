{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module RepositoryLib.Repository
  ( migrateDB
  , initConnectionPool
  , DBConnectionString
  , dotenvConnstr
  ) where

import Data.ByteString             (ByteString)
import Data.List
import Control.Concurrent
import Control.Exception           (bracket)
import Control.Monad.IO.Class
import Control.Applicative
import Data.Pool
import Database.PostgreSQL.Simple
import Servant
import Configuration.Dotenv        (parseFile)
import Data.Maybe                  (fromMaybe)
import Data.String                 (IsString(fromString))

type DBConnectionString = ByteString

migrateDB :: DBConnectionString -> IO ()
migrateDB connstr = bracket (connectPostgreSQL connstr) close $ \conn -> do
  execute_ conn $ 
       " CREATE TABLE IF NOT EXISTS Clientes"
    <> "   ( id     SERIAL PRIMARY KEY"
    <> "   , limite INTEGER NOT NULL"
    <> "   , saldo  INTEGER NOT NULL"
    <> "   );"

    <> " CREATE TABLE IF NOT EXISTS Transacoes"
    <> "   ( id           SERIAL PRIMARY KEY"
    <> "   , id_cliente   INTEGER NOT NULL"
    <> "   , valor        INTEGER NOT NULL"
    <> "   , tipo         VARCHAR(1) NOT NULL"
    <> "   , descricao    VARCHAR(10) NOT NULL"
    <> "   , realizada_em TIMESTAMPTZ NOT NULL DEFAULT NOW()"
    <> "   );"

    <> " CREATE OR REPLACE PROCEDURE inserir_transacao("
    <> "           xid_cliente  INTEGER"
    <> "   ,       xvalor       INTEGER"
    <> "   ,       xtipo        TEXT"
    <> "   ,       xdescricao   TEXT"
    <> "   , INOUT ysaldo_atual INTEGER DEFAULT NULL"
    <> "   , INOUT ylimite      INTEGER DEFAULT NULL"
    <> "   )"
    <> " LANGUAGE plpgsql"
    <> " AS $$"
    <> " BEGIN"
    <> "   WITH atualizar_saldo AS ("
    <> "     UPDATE Clientes SET saldo = saldo + xvalor"
    <> "       WHERE id = xid_cliente AND saldo + xvalor >= - limite"
    <> "       RETURNING saldo, limite"
    <> "   ),"
    <> "   nova_transacao AS ("
    <> "     INSERT INTO Transacoes (id_cliente,  valor,       tipo,  descricao)"
    <> "       SELECT                xid_cliente, ABS(xvalor), xtipo, xdescricao FROM atualizar_saldo"
    <> "   ) "
    <> "   SELECT saldo, limite"
    <> "     INTO ysaldo_atual, ylimite"
    <> "     FROM atualizar_saldo;"
    <> " END;"
    <> " $$;"

    <> " DO $$"
    <> " BEGIN"
    <> "   IF (SELECT id FROM Clientes LIMIT 1) IS NULL THEN"
    <> "     INSERT INTO Clientes (limite,   saldo)"
    <> "       VALUES             (100000,   0)" 
    <> "                        , (80000,    0)"
    <> "                        , (1000000,  0)"
    <> "                        , (10000000, 0)" 
    <> "                        , (500000,   0);"
    <> "   END IF;"
    <> " END;"
    <> " $$;"
  return ()

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

dotenvConnstr :: IO DBConnectionString
dotenvConnstr = do
  vars <- parseFile ".env"
  let dbHost =     lookup "POSTGRES_HOST"     vars
  let dbInstance = lookup "POSTGRES_INSTANCE" vars
  let dbUser =     lookup "POSTGRES_USER"     vars
  let dbPassword = lookup "POSTGRES_PASSWORD" vars
  let dbPort  =    Just (fromMaybe "5432" (lookup "POSTGRES_PORT" vars))
  let connstr = foldl
                  (\acc curt -> (<>) <$> acc <*> curt)
                  (Just mempty)
                  [ Just "host=",      dbHost
                  , Just " dbname=",   dbInstance
                  , Just " user=",     dbUser
                  , Just " password=", dbPassword
                  , Just " port=",     dbPort
                  ]
  
  case connstr of
    Nothing        -> fail "Falha ao ler as variáveis de conexão com banco de dados"
    Just okConnstr -> return (fromString okConnstr)
