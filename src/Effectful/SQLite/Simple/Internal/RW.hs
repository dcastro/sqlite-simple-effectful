{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_HADDOCK not-home #-}

module Effectful.SQLite.Simple.Internal.RW where

import Data.Int (Int64)
import Database.SQLite.Simple (FromRow, NamedParam, Query, ToRow)
import Database.SQLite.Simple qualified as S
import Database.SQLite.Simple.FromRow (RowParser)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Effectful.Dispatch.Static (seqUnliftIO, unsafeEff, unsafeEff_)
import GHC.Stack (HasCallStack)

----------------------------------------------------------------------------
-- Effect
----------------------------------------------------------------------------

data ConnMode = Read | Write

newtype Connection (mode :: ConnMode) = Connection {getConn :: S.Connection}

data SQLite :: Effect where
  WithReadConnection :: (Connection 'Read -> m a) -> SQLite m a
  WithWriteConnection :: (Connection 'Write -> m a) -> SQLite m a

type instance DispatchOf SQLite = 'Dynamic

withReadConnection :: (HasCallStack, SQLite :> es) => (Connection 'Read -> Eff es a) -> Eff es a
withReadConnection = send . WithReadConnection

withWriteConnection :: (HasCallStack, SQLite :> es) => (Connection 'Write -> Eff es a) -> Eff es a
withWriteConnection = send . WithWriteConnection

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

query :: (SQLite :> es) => (ToRow q, FromRow r) => Connection 'Read -> Query -> q -> Eff es [r]
query conn q params = unsafeEff_ $ S.query conn.getConn q params

query_ :: (SQLite :> es) => (FromRow r) => Connection 'Read -> Query -> Eff es [r]
query_ conn q = unsafeEff_ $ S.query_ conn.getConn q

queryWith :: (SQLite :> es) => (ToRow q) => RowParser r -> Connection 'Read -> Query -> q -> Eff es [r]
queryWith parser conn q params = unsafeEff_ $ S.queryWith parser conn.getConn q params

queryWith_ :: (SQLite :> es) => RowParser r -> Connection 'Read -> Query -> Eff es [r]
queryWith_ parser conn q = unsafeEff_ $ S.queryWith_ parser conn.getConn q

queryNamed :: (SQLite :> es) => (FromRow r) => Connection 'Read -> Query -> [NamedParam] -> Eff es [r]
queryNamed conn q params = unsafeEff_ $ S.queryNamed conn.getConn q params

lastInsertRowId :: (SQLite :> es) => Connection 'Read -> Eff es Int64
lastInsertRowId = unsafeEff_ . S.lastInsertRowId . getConn

changes :: (SQLite :> es) => Connection 'Read -> Eff es Int
changes = unsafeEff_ . S.changes . getConn

totalChanges :: (SQLite :> es) => Connection 'Read -> Eff es Int
totalChanges = unsafeEff_ . S.totalChanges . getConn

fold :: (SQLite :> es) => (FromRow row, ToRow params) => Connection 'Read -> Query -> params -> a -> (a -> row -> Eff es a) -> Eff es a
fold conn q params initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.fold conn.getConn q params initialState \a row ->
      unlift $ action a row

fold_ :: (SQLite :> es) => (FromRow row) => Connection 'Read -> Query -> a -> (a -> row -> Eff es a) -> Eff es a
fold_ conn q initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.fold_ conn.getConn q initialState \a row ->
      unlift $ action a row

foldNamed :: (SQLite :> es) => (FromRow row) => Connection 'Read -> Query -> [NamedParam] -> a -> (a -> row -> Eff es a) -> Eff es a
foldNamed conn q params initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.foldNamed conn.getConn q params initialState \a row ->
      unlift $ action a row

execute :: (SQLite :> es) => (ToRow q) => Connection 'Write -> Query -> q -> Eff es ()
execute conn q params = unsafeEff_ $ S.execute conn.getConn q params

execute_ :: (SQLite :> es) => Connection 'Write -> Query -> Eff es ()
execute_ conn q = unsafeEff_ $ S.execute_ conn.getConn q

executeMany :: (SQLite :> es) => (ToRow q) => Connection 'Write -> Query -> [q] -> Eff es ()
executeMany conn q params = unsafeEff_ $ S.executeMany conn.getConn q params

executeNamed :: (SQLite :> es) => Connection 'Write -> Query -> [NamedParam] -> Eff es ()
executeNamed conn q params = unsafeEff_ $ S.executeNamed conn.getConn q params

withTransaction :: (SQLite :> es) => Connection 'Read -> Eff es a -> Eff es a
withTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withTransaction conn.getConn $ unlift action

withImmediateTransaction :: (SQLite :> es) => Connection 'Write -> Eff es a -> Eff es a
withImmediateTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withImmediateTransaction conn.getConn $ unlift action

withExclusiveTransaction :: (SQLite :> es) => Connection 'Write -> Eff es a -> Eff es a
withExclusiveTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withExclusiveTransaction conn.getConn $ unlift action

withSavepoint :: (SQLite :> es) => Connection 'Write -> Eff es a -> Eff es a
withSavepoint conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withSavepoint conn.getConn $ unlift action

----------------------------------------------------------------------------
-- Interpreters
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

unsafeEffWithUnlift :: forall a es. (SQLite :> es) => ((forall x. Eff es x -> IO x) -> IO a) -> Eff es a
unsafeEffWithUnlift action =
  unsafeEff \env -> do
    seqUnliftIO env \unlift -> do
      action unlift
