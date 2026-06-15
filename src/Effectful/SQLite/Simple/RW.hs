{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- ORMOLU_DISABLE -}
{- | A dynamic effect that lets us use a __pooled__ SQLite `Database.SQLite.Simple.Connection`.

SQLite allows multiple connections to read/write concurrently, but concurrent writes will lead to contention, performance degradation, and @SQLITE_BUSY@ errors.
We avoid this by:

  * Having separate pools for reading and writing.
  * Configuring the write pool to have a maximum of 1 connection, thus serializing all writes.

__WARNING__: This interpreter sets the database's journal mode to [WAL](https://sqlite.org/wal.html),
so that readers will not block the writer and the writer will not block readers.

Note that even in WAL mode, [@SQLITE_BUSY@ errors can still occur](https://sqlite.org/wal.html#sometimes_queries_return_sqlite_busy_in_wal_mode).

To connect to multiple databases, see "Effectful.SQLite.Simple.RW.Labeled".

>>> import Effectful
>>> import Effectful.Concurrent (runConcurrent)
>>> import Effectful.SQLite.Simple.RW (SQLite)
>>> import Effectful.SQLite.Simple.RW qualified as SQL

>>> :{
app :: (SQLite :> es) => Eff es ()
app = do
  SQL.useWriteConnection \conn -> do
    SQL.withImmediateTransaction conn do
      SQL.query_ @User conn "SELECT * FROM users"
      SQL.execute conn "DELETE FROM users WHERE username = ?" (SQL.Only "dcastro")
:}

>>> :{
main :: IO ()
main = do
  let dbPath = "users.db"
  pools <-
    SQL.newPools
      =<< SQL.newPoolsConfig
        (SQL.open dbPath) -- Action to create a new read connection.
        60                -- Read connections' idle timeout in seconds.
        32                -- Max number of read connections.
        (SQL.open dbPath) -- Action to create a new write connection.
        60                -- Write connections' idle timeout in seconds.
  app
    & SQL.runSQLiteWithPools pools
    & runConcurrent
    & runEff
:}

-}
{- ORMOLU_ENABLE -}
module Effectful.SQLite.Simple.RW
  ( -- * Effects
    SQLite (..),

    -- * Use connection
    -- $useConnection
    useReadConnection,
    useWriteConnection,
    RWConnection (..),
    ConnMode (..),

    -- * Interpreters
    runSQLiteWithPools,
    Pools (..),
    newPools,
    PoolsConfig (..),
    newPoolsConfig,

    -- * Connections
    S.open,
    S.close,
    S.withConnection,
    S.setTrace,

    -- * Queries that return results
    query,
    query_,
    queryWith,
    queryWith_,
    queryNamed,
    lastInsertRowId,
    changes,
    totalChanges,

    -- * Queries that stream results
    fold,
    fold_,
    foldNamed,

    -- * Statements that do not return results
    execute,
    execute_,
    executeMany,
    executeNamed,
    S.field,

    -- * Transactions
    withTransaction,
    withImmediateTransaction,
    withExclusiveTransaction,
    withSavepoint,

    -- * Low-level statement API for stream access and prepared statements
    openStatement,
    closeStatement,
    withStatement,
    bind,
    bindNamed,
    reset,
    columnName,
    columnCount,
    withBind,
    nextRow,

    -- ** Exceptions
    S.FormatError (..),
    S.ResultError (..),
    S.SQLError (..),
    S.Error (..),

    -- * Types
    S.Query (..),
    S.Connection (..),
    S.ToRow (..),
    S.FromRow (..),
    S.Only (..),
    (S.:.) (..),
    S.SQLData (..),
    S.Statement (..),
    S.ColumnIndex (..),
    S.NamedParam (..),
  )
where

import Data.Function ((&))
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple (ColumnIndex, FromRow, NamedParam, Query, Statement, ToRow)
import Database.SQLite.Simple qualified as S
import Database.SQLite.Simple.FromRow (RowParser)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, send)
import Effectful.Dispatch.Static (seqUnliftIO, unsafeEff, unsafeEff_)
import GHC.Stack (HasCallStack)
import UnliftIO.Pool qualified as Pool

-- $setup
-- Clear all imports before running doctests
-- >>> :m
-- >>> :set -XOverloadedStrings
-- >>> import Data.Function ((&))
-- >>> import Data.Text (Text)
-- >>> import Effectful.SQLite.Simple (FromRow(fromRow))
-- >>> data User
-- >>> instance FromRow User where fromRow = undefined

----------------------------------------------------------------------------
-- Effect
----------------------------------------------------------------------------

{-
  NOTE: The rationale for having separate "read" and "write" pools has been documented here: @(ref:concurrency)
-}

-- | The mode in which a connection is acquired.
data ConnMode = Read | Write

-- | A connection that can be acquired in "read" or "write" mode.
-- This determines which kind of operations can be performed with the connection.
newtype RWConnection (mode :: ConnMode) = RWConnection {getConn :: S.Connection}

data SQLite :: Effect where
  UseReadConnection :: (RWConnection 'Read -> m a) -> SQLite m a
  UseWriteConnection :: (RWConnection 'Write -> m a) -> SQLite m a

type instance DispatchOf SQLite = 'Dynamic

{- ORMOLU_DISABLE -}
{- $useConnection

The `useReadConnection` and `useWriteConnection` operations retrieve a pooled connection from the context that can be used to run "read" or "write" operations.

If the action throws an exception of any type, the connection is closed and not returned to the pool.

__WARNING__:

* The connection must not be manually closed.
* The connection must not escape the scope of `useReadConnection` or `useWriteConnection`.
* `useWriteConnection` calls must not be nested.
* When `useWriteConnection` is used together with other locking primitives, the locks must always be acquired in the same order to avoid deadlocks.

-}
{- ORMOLU_ENABLE -}

-- | Retrieve the connection from the context and run the given "read" operations with it.
useReadConnection :: (HasCallStack, SQLite :> es) => (RWConnection 'Read -> Eff es a) -> Eff es a
useReadConnection = send . UseReadConnection

-- | Retrieve the connection from the context and run the given "read" or "write" operations with it.
useWriteConnection :: (HasCallStack, SQLite :> es) => (RWConnection 'Write -> Eff es a) -> Eff es a
useWriteConnection = send . UseWriteConnection

----------------------------------------------------------------------------
-- Interpreters
----------------------------------------------------------------------------

-- | Interprets the 'SQLite' effect by using 2 connection pools for reading and writing.
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

  action & interpret \env -> \case
    UseReadConnection action -> do
      localSeqUnlift env \unlift -> do
        Pool.withResource pools.readPool \conn -> do
          unlift $ action conn
    UseWriteConnection action -> do
      localSeqUnlift env \unlift -> do
        Pool.withResource pools.writePool \conn -> do
          unlift $ action conn

data Pools = Pools
  { readPool :: Pool.Pool (RWConnection 'Read),
    writePool :: Pool.Pool (RWConnection 'Write)
  }

newPools :: (MonadUnliftIO m) => PoolsConfig -> m Pools
newPools (PoolsConfig readPoolConfig writePoolConfig) = do
  readPool <- Pool.newPool readPoolConfig
  writePool <- Pool.newPool writePoolConfig
  pure Pools {readPool, writePool}

data PoolsConfig = PoolsConfig
  { readPoolConfig :: Pool.PoolConfig (RWConnection 'Read),
    writePoolConfig :: Pool.PoolConfig (RWConnection 'Write)
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
newPoolsConfig mkReadConn readTTLSeconds readMaxConns mkWriteConn writeTTLSeconds = do
  readPoolConfig <-
    Pool.mkDefaultPoolConfig
      (RWConnection @'Read <$> mkReadConn)
      (liftIO . S.close . getConn)
      readTTLSeconds
      readMaxConns

  let writeMaxConns = 1
  writePoolConfig <-
    Pool.mkDefaultPoolConfig
      (RWConnection @'Write <$> mkWriteConn)
      (liftIO . S.close . getConn)
      writeTTLSeconds
      writeMaxConns
  pure PoolsConfig {readPoolConfig, writePoolConfig}

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

query :: forall q r es mode. (SQLite :> es) => (ToRow q, FromRow r) => RWConnection mode -> Query -> q -> Eff es [r]
query conn q params = unsafeEff_ $ S.query conn.getConn q params

query_ :: forall r es mode. (SQLite :> es) => (FromRow r) => RWConnection mode -> Query -> Eff es [r]
query_ conn q = unsafeEff_ $ S.query_ conn.getConn q

queryWith :: forall q r es mode. (SQLite :> es) => (ToRow q) => RowParser r -> RWConnection mode -> Query -> q -> Eff es [r]
queryWith parser conn q params = unsafeEff_ $ S.queryWith parser conn.getConn q params

queryWith_ :: forall r es mode. (SQLite :> es) => RowParser r -> RWConnection mode -> Query -> Eff es [r]
queryWith_ parser conn q = unsafeEff_ $ S.queryWith_ parser conn.getConn q

queryNamed :: forall r es mode. (SQLite :> es) => (FromRow r) => RWConnection mode -> Query -> [NamedParam] -> Eff es [r]
queryNamed conn q params = unsafeEff_ $ S.queryNamed conn.getConn q params

lastInsertRowId :: forall es mode. (SQLite :> es) => RWConnection mode -> Eff es Int64
lastInsertRowId = unsafeEff_ . S.lastInsertRowId . getConn

changes :: forall es mode. (SQLite :> es) => RWConnection mode -> Eff es Int
changes = unsafeEff_ . S.changes . getConn

totalChanges :: forall es mode. (SQLite :> es) => RWConnection mode -> Eff es Int
totalChanges = unsafeEff_ . S.totalChanges . getConn

fold :: forall row params a es mode. (SQLite :> es) => (FromRow row, ToRow params) => RWConnection mode -> Query -> params -> a -> (a -> row -> Eff es a) -> Eff es a
fold conn q params initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.fold conn.getConn q params initialState \a row ->
      unlift $ action a row

fold_ :: forall row a es mode. (SQLite :> es) => (FromRow row) => RWConnection mode -> Query -> a -> (a -> row -> Eff es a) -> Eff es a
fold_ conn q initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.fold_ conn.getConn q initialState \a row ->
      unlift $ action a row

foldNamed :: forall row a es mode. (SQLite :> es) => (FromRow row) => RWConnection mode -> Query -> [NamedParam] -> a -> (a -> row -> Eff es a) -> Eff es a
foldNamed conn q params initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.foldNamed conn.getConn q params initialState \a row ->
      unlift $ action a row

execute :: forall q es. (SQLite :> es) => (ToRow q) => RWConnection 'Write -> Query -> q -> Eff es ()
execute conn q params = unsafeEff_ $ S.execute conn.getConn q params

execute_ :: forall es. (SQLite :> es) => RWConnection 'Write -> Query -> Eff es ()
execute_ conn q = unsafeEff_ $ S.execute_ conn.getConn q

executeMany :: forall q es. (SQLite :> es) => (ToRow q) => RWConnection 'Write -> Query -> [q] -> Eff es ()
executeMany conn q params = unsafeEff_ $ S.executeMany conn.getConn q params

executeNamed :: forall es. (SQLite :> es) => RWConnection 'Write -> Query -> [NamedParam] -> Eff es ()
executeNamed conn q params = unsafeEff_ $ S.executeNamed conn.getConn q params

withTransaction :: forall a es mode. (SQLite :> es) => RWConnection mode -> Eff es a -> Eff es a
withTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withTransaction conn.getConn $ unlift action

withImmediateTransaction :: forall a es. (SQLite :> es) => RWConnection 'Write -> Eff es a -> Eff es a
withImmediateTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withImmediateTransaction conn.getConn $ unlift action

withExclusiveTransaction :: forall a es. (SQLite :> es) => RWConnection 'Write -> Eff es a -> Eff es a
withExclusiveTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withExclusiveTransaction conn.getConn $ unlift action

withSavepoint :: forall a es. (SQLite :> es) => RWConnection 'Write -> Eff es a -> Eff es a
withSavepoint conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withSavepoint conn.getConn $ unlift action

openStatement :: forall es. (SQLite :> es) => RWConnection 'Write -> Query -> Eff es Statement
openStatement conn q = unsafeEff_ $ S.openStatement conn.getConn q

closeStatement :: forall es. (SQLite :> es) => Statement -> Eff es ()
closeStatement stmt = unsafeEff_ $ S.closeStatement stmt

withStatement :: forall a es. (SQLite :> es) => RWConnection 'Write -> Query -> (Statement -> Eff es a) -> Eff es a
withStatement conn q action =
  unsafeEffWithUnlift \unlift -> do
    S.withStatement conn.getConn q (unlift . action)

bind :: forall params es. (SQLite :> es) => (ToRow params) => Statement -> params -> Eff es ()
bind stmt params = unsafeEff_ $ S.bind stmt params

bindNamed :: forall es. (SQLite :> es) => Statement -> [NamedParam] -> Eff es ()
bindNamed stmt params = unsafeEff_ $ S.bindNamed stmt params

reset :: forall es. (SQLite :> es) => Statement -> Eff es ()
reset stmt = unsafeEff_ $ S.reset stmt

columnName :: forall es. (SQLite :> es) => Statement -> ColumnIndex -> Eff es Text
columnName stmt idx = unsafeEff_ $ S.columnName stmt idx

columnCount :: forall es. (SQLite :> es) => Statement -> Eff es ColumnIndex
columnCount stmt = unsafeEff_ $ S.columnCount stmt

withBind :: forall params a es. (SQLite :> es) => (ToRow params) => Statement -> params -> Eff es a -> Eff es a
withBind stmt params action =
  unsafeEffWithUnlift \unlift -> do
    S.withBind stmt params $ unlift action

nextRow :: forall r es. (SQLite :> es) => (FromRow r) => Statement -> Eff es (Maybe r)
nextRow stmt = unsafeEff_ $ S.nextRow stmt

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

unsafeEffWithUnlift :: forall a es. (SQLite :> es) => ((forall x. Eff es x -> IO x) -> IO a) -> Eff es a
unsafeEffWithUnlift action =
  unsafeEff \env -> do
    seqUnliftIO env \unlift -> do
      action unlift
