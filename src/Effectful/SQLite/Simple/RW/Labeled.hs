{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.SQLite.Simple.RW.Labeled where

import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple (ColumnIndex, FromRow, NamedParam, Query, Statement, ToRow)
import Database.SQLite.Simple qualified as S
import Database.SQLite.Simple.FromRow (RowParser)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Effectful.Dispatch.Static (seqUnliftIO, unsafeEff, unsafeEff_)
import Effectful.Labeled (Labeled (..), runLabeled)
import Effectful.SQLite.Simple.RW (ConnMode (..), SQLite)
import Effectful.SQLite.Simple.RW qualified as RW
import GHC.Stack (HasCallStack)

----------------------------------------------------------------------------
-- Effect
----------------------------------------------------------------------------

{-
  The rationale for having separate "read" and "write" pools has been documented here: @(ref:concurrency)
-}

newtype LRWConnection (label :: k) (mode :: RW.ConnMode) = LRWConnection {getConn :: RW.RWConnection mode}

useReadConnection :: forall label es a. (HasCallStack, Labeled label SQLite :> es) => (LRWConnection label 'Read -> Eff es a) -> Eff es a
useReadConnection use = send $ Labeled @label $ RW.UseReadConnection \conn -> use (LRWConnection conn)

useWriteConnection :: forall label es a. (HasCallStack, Labeled label SQLite :> es) => (LRWConnection label 'Write -> Eff es a) -> Eff es a
useWriteConnection use = send $ Labeled @label $ RW.UseWriteConnection \conn -> use (LRWConnection conn)

----------------------------------------------------------------------------
-- Interpreters
----------------------------------------------------------------------------

-- | Interprets the 'SQLite' effect by using 2 connection pools for reading and writing.
--
-- SQLite allows multiple connections to read/write concurrently, but concurrent writes will lead to @SQLITE_BUSY@ errors.
-- We avoid this by:
--
--   * Having separate pools for reading and writing.
--   * Configuring the write pool to have a maximum of 1 connection, thus serializing all writes.
--
-- __WARNING__: This interpreter sets the database's journal mode to [WAL](https://sqlite.org/wal.html),
-- so that readers will not block the writer and the writer will not block readers.
--
-- Note that even in WAL mode, [@SQLITE_BUSY@ errors can still occur](https://sqlite.org/wal.html#sometimes_queries_return_sqlite_busy_in_wal_mode).
runSQLiteWithPools ::
  forall label es a.
  (HasCallStack, IOE :> es) =>
  RW.Pools ->
  Eff (Labeled label SQLite ': es) a ->
  Eff es a
runSQLiteWithPools = runLabeled @label . RW.runSQLiteWithPools

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

query :: (Labeled label SQLite :> es) => (ToRow q, FromRow r) => LRWConnection label mode -> Query -> q -> Eff es [r]
query conn q params = unsafeEff_ $ S.query conn.getConn.getConn q params

query_ :: (Labeled label SQLite :> es) => (FromRow r) => LRWConnection label mode -> Query -> Eff es [r]
query_ conn q = unsafeEff_ $ S.query_ conn.getConn.getConn q

queryWith :: (Labeled label SQLite :> es) => (ToRow q) => RowParser r -> LRWConnection label mode -> Query -> q -> Eff es [r]
queryWith parser conn q params = unsafeEff_ $ S.queryWith parser conn.getConn.getConn q params

queryWith_ :: (Labeled label SQLite :> es) => RowParser r -> LRWConnection label mode -> Query -> Eff es [r]
queryWith_ parser conn q = unsafeEff_ $ S.queryWith_ parser conn.getConn.getConn q

queryNamed :: (Labeled label SQLite :> es) => (FromRow r) => LRWConnection label mode -> Query -> [NamedParam] -> Eff es [r]
queryNamed conn q params = unsafeEff_ $ S.queryNamed conn.getConn.getConn q params

lastInsertRowId :: (Labeled label SQLite :> es) => LRWConnection label mode -> Eff es Int64
lastInsertRowId conn = unsafeEff_ $ S.lastInsertRowId conn.getConn.getConn

changes :: (Labeled label SQLite :> es) => LRWConnection label mode -> Eff es Int
changes conn = unsafeEff_ $ S.changes conn.getConn.getConn

totalChanges :: (Labeled label SQLite :> es) => LRWConnection label mode -> Eff es Int
totalChanges conn = unsafeEff_ $ S.totalChanges conn.getConn.getConn

fold :: forall label row params a es mode. (Labeled label SQLite :> es) => (FromRow row, ToRow params) => LRWConnection label mode -> Query -> params -> a -> (a -> row -> Eff es a) -> Eff es a
fold conn q params initialState action =
  unsafeEffWithUnlift @label \unlift -> do
    S.fold conn.getConn.getConn q params initialState \a row ->
      unlift $ action a row

fold_ :: forall label row a es mode. (Labeled label SQLite :> es) => (FromRow row) => LRWConnection label mode -> Query -> a -> (a -> row -> Eff es a) -> Eff es a
fold_ conn q initialState action =
  unsafeEffWithUnlift @label \unlift -> do
    S.fold_ conn.getConn.getConn q initialState \a row ->
      unlift $ action a row

foldNamed :: forall label row a es mode. (Labeled label SQLite :> es) => (FromRow row) => LRWConnection label mode -> Query -> [NamedParam] -> a -> (a -> row -> Eff es a) -> Eff es a
foldNamed conn q params initialState action =
  unsafeEffWithUnlift @label \unlift -> do
    S.foldNamed conn.getConn.getConn q params initialState \a row ->
      unlift $ action a row

execute :: (Labeled label SQLite :> es) => (ToRow q) => LRWConnection label 'Write -> Query -> q -> Eff es ()
execute conn q params = unsafeEff_ $ S.execute conn.getConn.getConn q params

execute_ :: (Labeled label SQLite :> es) => LRWConnection label 'Write -> Query -> Eff es ()
execute_ conn q = unsafeEff_ $ S.execute_ conn.getConn.getConn q

executeMany :: (Labeled label SQLite :> es) => (ToRow q) => LRWConnection label 'Write -> Query -> [q] -> Eff es ()
executeMany conn q params = unsafeEff_ $ S.executeMany conn.getConn.getConn q params

executeNamed :: (Labeled label SQLite :> es) => LRWConnection label 'Write -> Query -> [NamedParam] -> Eff es ()
executeNamed conn q params = unsafeEff_ $ S.executeNamed conn.getConn.getConn q params

withTransaction :: forall label a es mode. (Labeled label SQLite :> es) => LRWConnection label mode -> Eff es a -> Eff es a
withTransaction conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withTransaction conn.getConn.getConn $ unlift action

withImmediateTransaction :: forall label a es. (Labeled label SQLite :> es) => LRWConnection label 'Write -> Eff es a -> Eff es a
withImmediateTransaction conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withImmediateTransaction conn.getConn.getConn $ unlift action

withExclusiveTransaction :: forall label a es. (Labeled label SQLite :> es) => LRWConnection label 'Write -> Eff es a -> Eff es a
withExclusiveTransaction conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withExclusiveTransaction conn.getConn.getConn $ unlift action

withSavepoint :: forall label a es. (Labeled label SQLite :> es) => LRWConnection label 'Write -> Eff es a -> Eff es a
withSavepoint conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withSavepoint conn.getConn.getConn $ unlift action

openStatement :: (Labeled label SQLite :> es) => LRWConnection label 'Write -> Query -> Eff es Statement
openStatement conn q = unsafeEff_ $ S.openStatement conn.getConn.getConn q

closeStatement :: (Labeled label SQLite :> es) => Statement -> Eff es ()
closeStatement stmt = unsafeEff_ $ S.closeStatement stmt

withStatement :: forall label a es. (Labeled label SQLite :> es) => LRWConnection label 'Write -> Query -> (Statement -> Eff es a) -> Eff es a
withStatement conn q action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withStatement conn.getConn.getConn q (unlift . action)

bind :: (Labeled label SQLite :> es) => (ToRow params) => Statement -> params -> Eff es ()
bind stmt params = unsafeEff_ $ S.bind stmt params

bindNamed :: (Labeled label SQLite :> es) => Statement -> [NamedParam] -> Eff es ()
bindNamed stmt params = unsafeEff_ $ S.bindNamed stmt params

reset :: (Labeled label SQLite :> es) => Statement -> Eff es ()
reset stmt = unsafeEff_ $ S.reset stmt

columnName :: (Labeled label SQLite :> es) => Statement -> ColumnIndex -> Eff es Text
columnName stmt idx = unsafeEff_ $ S.columnName stmt idx

columnCount :: (Labeled label SQLite :> es) => Statement -> Eff es ColumnIndex
columnCount stmt = unsafeEff_ $ S.columnCount stmt

withBind :: forall label params a es. (Labeled label SQLite :> es) => (ToRow params) => Statement -> params -> Eff es a -> Eff es a
withBind stmt params action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withBind stmt params $ unlift action

nextRow :: (Labeled label SQLite :> es) => (FromRow r) => Statement -> Eff es (Maybe r)
nextRow stmt = unsafeEff_ $ S.nextRow stmt

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

unsafeEffWithUnlift :: forall label a es. (Labeled label SQLite :> es) => ((forall x. Eff es x -> IO x) -> IO a) -> Eff es a
unsafeEffWithUnlift action =
  unsafeEff \env -> do
    seqUnliftIO env \unlift -> do
      action unlift
