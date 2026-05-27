{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK not-home #-}

module Effectful.SQLite.Simple.Internal.RW where

import Data.Int (Int64)
import Database.SQLite.Simple (FromRow, NamedParam, Query, ToRow)
import Database.SQLite.Simple qualified as S
import Database.SQLite.Simple.FromRow (RowParser)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, send)
import Effectful.Dispatch.Static (seqUnliftIO, unsafeEff, unsafeEff_)
import GHC.Stack (HasCallStack)
import UnliftIO.Pool qualified as Pool

----------------------------------------------------------------------------
-- Effect
----------------------------------------------------------------------------

{-
  The rationale for having separate "read" and "write" pools has been documented here: @(ref:concurrency)
-}

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
runSQLiteWithPools :: (HasCallStack, IOE :> es) => Pools -> Eff (SQLite ': es) a -> Eff es a
runSQLiteWithPools pools action = do
  {-
    NOTE: we set the WAL mode upfront.

    To avoid SQLITE_BUSY errors, all connections must be made in WAL mode.
    We can't set WAL mode lazily (e.g. every time a pool creates a new connection),
    because having the "read pool" execute `PRAGMA journal_mode=WAL` would cause it to acquire
    an exclusive lock on the database, which it must not do.
    Therefore, we must do it eagerly, here.
  -}
  Pool.withResource pools.writePool \conn -> do
    liftIO $ S.execute_ conn.getConn "PRAGMA journal_mode=WAL"

  interpret
    ( \env -> \case
        WithReadConnection action -> do
          localSeqUnlift env \unlift -> do
            Pool.withResource pools.readPool \conn -> do
              unlift $ action conn
        WithWriteConnection action -> do
          localSeqUnlift env \unlift -> do
            Pool.withResource pools.writePool \conn -> do
              unlift $ action conn
    )
    action

data Pools = Pools
  { readPool :: Pool.Pool (Connection 'Read),
    writePool :: Pool.Pool (Connection 'Write)
  }

newPools :: (MonadUnliftIO m) => PoolsConfig -> m Pools
newPools (PoolsConfig readPoolConfig writePoolConfig) = do
  readPool <- Pool.newPool readPoolConfig
  writePool <- Pool.newPool writePoolConfig
  pure Pools {readPool, writePool}

data PoolsConfig = PoolsConfig
  { readPoolConfig :: Pool.PoolConfig (Connection 'Read),
    writePoolConfig :: Pool.PoolConfig (Connection 'Write)
  }

newPoolsConfig ::
  (MonadUnliftIO m) =>
  -- | The action to create a new connection for reading from the database.
  m S.Connection ->
  -- | The number of seconds for which an unused read connection is kept around. The smallest acceptable value is 0.5.
  --
  -- Note: the elapsed time before destroying a connection may be a little longer than requested, as the collector thread wakes at 1-second intervals.
  Double ->
  -- | The maximum number of read connections to keep open at once. The smallest acceptable value is 1.
  Int ->
  -- | The action to create a new connection for writing to the database.
  m S.Connection ->
  -- | The number of seconds for which an unused write connection is kept around. The smallest acceptable value is 0.5.
  --
  -- Note: the elapsed time before destroying a connection may be a little longer than requested, as the collector thread wakes at 1-second intervals.
  Double ->
  m PoolsConfig
newPoolsConfig mkReadConn readTTLSeconds readMaxResources mkWriteConn writeTTLSeconds = do
  readPoolConfig <-
    Pool.mkDefaultPoolConfig
      (Connection @'Read <$> mkReadConn)
      (liftIO . S.close . getConn)
      readTTLSeconds
      readMaxResources

  let writeMaxConns = 1
  writePoolConfig <-
    Pool.mkDefaultPoolConfig
      (Connection @'Write <$> mkWriteConn)
      (liftIO . S.close . getConn)
      writeTTLSeconds
      writeMaxConns
  pure PoolsConfig {readPoolConfig, writePoolConfig}

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

query :: (SQLite :> es) => (ToRow q, FromRow r) => Connection mode -> Query -> q -> Eff es [r]
query conn q params = unsafeEff_ $ S.query conn.getConn q params

query_ :: (SQLite :> es) => (FromRow r) => Connection mode -> Query -> Eff es [r]
query_ conn q = unsafeEff_ $ S.query_ conn.getConn q

queryWith :: (SQLite :> es) => (ToRow q) => RowParser r -> Connection mode -> Query -> q -> Eff es [r]
queryWith parser conn q params = unsafeEff_ $ S.queryWith parser conn.getConn q params

queryWith_ :: (SQLite :> es) => RowParser r -> Connection mode -> Query -> Eff es [r]
queryWith_ parser conn q = unsafeEff_ $ S.queryWith_ parser conn.getConn q

queryNamed :: (SQLite :> es) => (FromRow r) => Connection mode -> Query -> [NamedParam] -> Eff es [r]
queryNamed conn q params = unsafeEff_ $ S.queryNamed conn.getConn q params

lastInsertRowId :: (SQLite :> es) => Connection mode -> Eff es Int64
lastInsertRowId = unsafeEff_ . S.lastInsertRowId . getConn

changes :: (SQLite :> es) => Connection mode -> Eff es Int
changes = unsafeEff_ . S.changes . getConn

totalChanges :: (SQLite :> es) => Connection mode -> Eff es Int
totalChanges = unsafeEff_ . S.totalChanges . getConn

fold :: (SQLite :> es) => (FromRow row, ToRow params) => Connection mode -> Query -> params -> a -> (a -> row -> Eff es a) -> Eff es a
fold conn q params initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.fold conn.getConn q params initialState \a row ->
      unlift $ action a row

fold_ :: (SQLite :> es) => (FromRow row) => Connection mode -> Query -> a -> (a -> row -> Eff es a) -> Eff es a
fold_ conn q initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.fold_ conn.getConn q initialState \a row ->
      unlift $ action a row

foldNamed :: (SQLite :> es) => (FromRow row) => Connection mode -> Query -> [NamedParam] -> a -> (a -> row -> Eff es a) -> Eff es a
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

withTransaction :: (SQLite :> es) => Connection mode -> Eff es a -> Eff es a
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
-- Utils
----------------------------------------------------------------------------

unsafeEffWithUnlift :: forall a es. (SQLite :> es) => ((forall x. Eff es x -> IO x) -> IO a) -> Eff es a
unsafeEffWithUnlift action =
  unsafeEff \env -> do
    seqUnliftIO env \unlift -> do
      action unlift
