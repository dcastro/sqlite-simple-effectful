{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- ORMOLU_DISABLE -}
{- | A __pooled__ `SQLite` effect with a label attached, allowing multiple SQLite databases to be used in the same program.

SQLite allows multiple connections to read/write concurrently, but concurrent writes will lead to contention and performance degradation, and @SQLITE_BUSY@ errors.
We avoid this by:
  * Having separate pools for reading and writing.
  * Configuring the write pool to have a maximum of 1 connection, thus serializing all writes.

__WARNING__: This interpreter sets the database's journal mode to [WAL](https://sqlite.org/wal.html),
so that readers will not block the writer and the writer will not block readers.

Note that even in WAL mode, [@SQLITE_BUSY@ errors can still occur](https://sqlite.org/wal.html#sometimes_queries_return_sqlite_busy_in_wal_mode).

>>> import Effectful
>>> import Effectful.Concurrent (runConcurrent)
>>> import Effectful.SQLite.Simple.RW.Labeled (Labeled, SQLite)
>>> import Effectful.SQLite.Simple.RW.Labeled qualified as SQL

>>> :{
app ::
  (Labeled "users" SQLite :> es)  =>
  (Labeled "products" SQLite :> es)  =>
  Eff es ()
app = do
  users <- SQL.useWriteConnection @"users" \usersConn -> do
    SQL.execute usersConn "DELETE FROM users WHERE username = ?" (SQL.Only "dcastro")
  products <- SQL.useReadConnection @"products" \productsConn -> do
    SQL.query_ @_ @_ @Product productsConn "SELECT * FROM products"
  pure ()
:}

>>> :{
main :: IO ()
main = do
  let mkPools dbPath =
        SQL.newPools
          =<< SQL.newPoolsConfig
            (SQL.open dbPath) -- Action to create a new read connection.
            60                -- Read connections' idle timeout in seconds.
            32                -- Max number of read connections.
            (SQL.open dbPath) -- Action to create a new write connection.
            60                -- Write connections' idle timeout in seconds.
  userPools <- mkPools "users.db"
  productPools <- mkPools "products.db"
  app
    & SQL.runSQLiteWithPools @"users" userPools
    & SQL.runSQLiteWithPools @"products" productPools
    & runConcurrent
    & runEff
:}

-}
{- ORMOLU_ENABLE -}
module Effectful.SQLite.Simple.RW.Labeled
  ( -- * Effects
    Labeled,
    SQLite (..),

    -- * Use connection
    -- $useConnection
    useReadConnection,
    useWriteConnection,
    LRWConnection (..),
    ConnMode (..),

    -- * Interpreters
    runSQLiteWithPools,
    RW.Pools (..),
    RW.newPools,
    RW.PoolsConfig (..),
    RW.newPoolsConfig,

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

-- $setup
-- Clear all imports before running doctests
-- >>> :m
-- >>> :set -XOverloadedStrings
-- >>> import Data.Function ((&))
-- >>> import Effectful.SQLite.Simple (FromRow(fromRow))
-- >>> data User
-- >>> instance FromRow User where fromRow = undefined
-- >>> data Product
-- >>> instance FromRow Product where fromRow = undefined

----------------------------------------------------------------------------
-- Effect
----------------------------------------------------------------------------

{-
  The rationale for having separate "read" and "write" pools has been documented here: @(ref:concurrency)
-}

-- | A labelled connection that can be acquired in "read" or "write" mode.
-- This determines which kind of operations can be performed with the connection.
newtype LRWConnection (label :: k) (mode :: RW.ConnMode) = LRWConnection {getConn :: RW.RWConnection mode}

{- ORMOLU_DISABLE -}
{- $useConnection

The `useReadConnection` and `useWriteConnection` operations retrieve a pooled connection from the context that can be used to run "read" or "write" operations.

If the action throws an exception of any type, the connection is closed and not returned to the pool.

__WARNING__:

* The connection must not be manually closed.
* The connection must not escape the scope of `useReadConnection` or `useWriteConnection`.
* `useWriteConnection` calls must not be nested.
* When `useWriteConnection` is used together with other locking primitives, the locks must always be acquired in the same order to avoid deadlocks.

E.g., in the example below, the "write" connections to the 2 databases are acquired out of order, which could lead to a deadlock.

>>> :{
import Effectful
import Effectful.SQLite.Simple.RW.Labeled (Labeled, SQLite)
import Effectful.SQLite.Simple.RW.Labeled qualified as SQL
f, g :: (Labeled "users" SQLite :> es, Labeled "products" SQLite :> es) => Eff es ()
f = do
  SQL.useWriteConnection @"users" \usersConn -> do
    SQL.useWriteConnection @"products" \productsConn -> do
      pure ()
g = do
  SQL.useWriteConnection @"products" \productsConn -> do
   SQL.useWriteConnection @"users" \usersConn -> do
      pure ()
:}

-}
{- ORMOLU_ENABLE -}

-- | Retrieve the connection from the context and run the given "read" operations with it.
useReadConnection :: forall label es a. (HasCallStack, Labeled label SQLite :> es) => (LRWConnection label 'Read -> Eff es a) -> Eff es a
useReadConnection use = send $ Labeled @label $ RW.UseReadConnection \conn -> use (LRWConnection conn)

-- | Retrieve the connection from the context and run the given "read" or "write" operations with it.
useWriteConnection :: forall label es a. (HasCallStack, Labeled label SQLite :> es) => (LRWConnection label 'Write -> Eff es a) -> Eff es a
useWriteConnection use = send $ Labeled @label $ RW.UseWriteConnection \conn -> use (LRWConnection conn)

----------------------------------------------------------------------------
-- Interpreters
----------------------------------------------------------------------------

-- | Interprets the 'SQLite' effect by using 2 connection pools for reading and writing.
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

query :: forall q r label mode es. (Labeled label SQLite :> es) => (ToRow q, FromRow r) => LRWConnection label mode -> Query -> q -> Eff es [r]
query conn q params = unsafeEff_ $ S.query conn.getConn.getConn q params

query_ :: forall r label mode es. (Labeled label SQLite :> es) => (FromRow r) => LRWConnection label mode -> Query -> Eff es [r]
query_ conn q = unsafeEff_ $ S.query_ conn.getConn.getConn q

queryWith :: forall q r label mode es. (Labeled label SQLite :> es) => (ToRow q) => RowParser r -> LRWConnection label mode -> Query -> q -> Eff es [r]
queryWith parser conn q params = unsafeEff_ $ S.queryWith parser conn.getConn.getConn q params

queryWith_ :: forall r label mode es. (Labeled label SQLite :> es) => RowParser r -> LRWConnection label mode -> Query -> Eff es [r]
queryWith_ parser conn q = unsafeEff_ $ S.queryWith_ parser conn.getConn.getConn q

queryNamed :: forall r label mode es. (Labeled label SQLite :> es) => (FromRow r) => LRWConnection label mode -> Query -> [NamedParam] -> Eff es [r]
queryNamed conn q params = unsafeEff_ $ S.queryNamed conn.getConn.getConn q params

lastInsertRowId :: forall label mode es. (Labeled label SQLite :> es) => LRWConnection label mode -> Eff es Int64
lastInsertRowId conn = unsafeEff_ $ S.lastInsertRowId conn.getConn.getConn

changes :: forall label mode es. (Labeled label SQLite :> es) => LRWConnection label mode -> Eff es Int
changes conn = unsafeEff_ $ S.changes conn.getConn.getConn

totalChanges :: forall label mode es. (Labeled label SQLite :> es) => LRWConnection label mode -> Eff es Int
totalChanges conn = unsafeEff_ $ S.totalChanges conn.getConn.getConn

fold :: forall row params a label mode es. (Labeled label SQLite :> es) => (FromRow row, ToRow params) => LRWConnection label mode -> Query -> params -> a -> (a -> row -> Eff es a) -> Eff es a
fold conn q params initialState action =
  unsafeEffWithUnlift @label \unlift -> do
    S.fold conn.getConn.getConn q params initialState \a row ->
      unlift $ action a row

fold_ :: forall row a label mode es. (Labeled label SQLite :> es) => (FromRow row) => LRWConnection label mode -> Query -> a -> (a -> row -> Eff es a) -> Eff es a
fold_ conn q initialState action =
  unsafeEffWithUnlift @label \unlift -> do
    S.fold_ conn.getConn.getConn q initialState \a row ->
      unlift $ action a row

foldNamed :: forall row a label mode es. (Labeled label SQLite :> es) => (FromRow row) => LRWConnection label mode -> Query -> [NamedParam] -> a -> (a -> row -> Eff es a) -> Eff es a
foldNamed conn q params initialState action =
  unsafeEffWithUnlift @label \unlift -> do
    S.foldNamed conn.getConn.getConn q params initialState \a row ->
      unlift $ action a row

execute :: forall q label es. (Labeled label SQLite :> es) => (ToRow q) => LRWConnection label 'Write -> Query -> q -> Eff es ()
execute conn q params = unsafeEff_ $ S.execute conn.getConn.getConn q params

execute_ :: forall label es. (Labeled label SQLite :> es) => LRWConnection label 'Write -> Query -> Eff es ()
execute_ conn q = unsafeEff_ $ S.execute_ conn.getConn.getConn q

executeMany :: forall q label es. (Labeled label SQLite :> es) => (ToRow q) => LRWConnection label 'Write -> Query -> [q] -> Eff es ()
executeMany conn q params = unsafeEff_ $ S.executeMany conn.getConn.getConn q params

executeNamed :: forall label es. (Labeled label SQLite :> es) => LRWConnection label 'Write -> Query -> [NamedParam] -> Eff es ()
executeNamed conn q params = unsafeEff_ $ S.executeNamed conn.getConn.getConn q params

withTransaction :: forall a label mode es. (Labeled label SQLite :> es) => LRWConnection label mode -> Eff es a -> Eff es a
withTransaction conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withTransaction conn.getConn.getConn $ unlift action

withImmediateTransaction :: forall a label es. (Labeled label SQLite :> es) => LRWConnection label 'Write -> Eff es a -> Eff es a
withImmediateTransaction conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withImmediateTransaction conn.getConn.getConn $ unlift action

withExclusiveTransaction :: forall a label es. (Labeled label SQLite :> es) => LRWConnection label 'Write -> Eff es a -> Eff es a
withExclusiveTransaction conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withExclusiveTransaction conn.getConn.getConn $ unlift action

withSavepoint :: forall a label es. (Labeled label SQLite :> es) => LRWConnection label 'Write -> Eff es a -> Eff es a
withSavepoint conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withSavepoint conn.getConn.getConn $ unlift action

openStatement :: forall label es. (Labeled label SQLite :> es) => LRWConnection label 'Write -> Query -> Eff es Statement
openStatement conn q = unsafeEff_ $ S.openStatement conn.getConn.getConn q

closeStatement :: forall label es. (Labeled label SQLite :> es) => Statement -> Eff es ()
closeStatement stmt = unsafeEff_ $ S.closeStatement stmt

withStatement :: forall a label es. (Labeled label SQLite :> es) => LRWConnection label 'Write -> Query -> (Statement -> Eff es a) -> Eff es a
withStatement conn q action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withStatement conn.getConn.getConn q (unlift . action)

bind :: forall label params es. (Labeled label SQLite :> es) => (ToRow params) => Statement -> params -> Eff es ()
bind stmt params = unsafeEff_ $ S.bind stmt params

bindNamed :: forall label es. (Labeled label SQLite :> es) => Statement -> [NamedParam] -> Eff es ()
bindNamed stmt params = unsafeEff_ $ S.bindNamed stmt params

reset :: forall label es. (Labeled label SQLite :> es) => Statement -> Eff es ()
reset stmt = unsafeEff_ $ S.reset stmt

columnName :: forall label es. (Labeled label SQLite :> es) => Statement -> ColumnIndex -> Eff es Text
columnName stmt idx = unsafeEff_ $ S.columnName stmt idx

columnCount :: forall label es. (Labeled label SQLite :> es) => Statement -> Eff es ColumnIndex
columnCount stmt = unsafeEff_ $ S.columnCount stmt

withBind :: forall label params a es. (Labeled label SQLite :> es) => (ToRow params) => Statement -> params -> Eff es a -> Eff es a
withBind stmt params action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withBind stmt params $ unlift action

nextRow :: forall label r es. (Labeled label SQLite :> es) => (FromRow r) => Statement -> Eff es (Maybe r)
nextRow stmt = unsafeEff_ $ S.nextRow stmt

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

unsafeEffWithUnlift :: forall label a es. (Labeled label SQLite :> es) => ((forall x. Eff es x -> IO x) -> IO a) -> Eff es a
unsafeEffWithUnlift action =
  unsafeEff \env -> do
    seqUnliftIO env \unlift -> do
      action unlift
