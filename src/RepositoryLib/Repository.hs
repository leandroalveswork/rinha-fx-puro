{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module RepositoryLib.Repository
  ( migrateDB
  , initConnectionPool
  , DBConnectionString
  ) where

import Data.ByteString             (ByteString)
import Control.Concurrent
import Control.Exception           (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Servant

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
    <> "   , realizada_em TIMESTAMP NOT NULL DEFAULT NOW()"
    <> "   );"

    <> " CREATE OR REPLACE PROCEDURE inserir_transacao("
    <> "           id_cliente  INTEGER"
    <> "   ,       valor       INTEGER"
    <> "   ,       tipo        TEXT"
    <> "   ,       descricao   TEXT"
    <> "   , INOUT saldo_atual INTEGER DEFAULT NULL"
    <> "   , INOUT limite      INTEGER DEFAULT NULL"
    <> "   )"
    <> " LANGUAGE plpgsql"
    <> " AS $$"
    <> " BEGIN"
    <> "   WITH atualizar_saldo AS ("
    <> "     UPDATE Transacoes SET saldo = saldo + valor"
    <> "       WHERE Transacoes.id_cliente = id_cliente AND saldo + valor >= - limite"
    <> "       RETURNING saldo, limite"
    <> "   ),"
    <> "   nova_transacao AS ("
    <> "     INSERT INTO Transacoes (id_cliente, valor,      tipo, descricao)"
    <> "       SELECT                id_cliente, ABS(valor), tipo, descricao FROM atualizar_saldo"
    <> "   ) "
    <> "   SELECT saldo, limite"
    <> "     INTO saldo_atual, limite"
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

