{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- ORMOLU_DISABLE -}
{- | A `SQLite` effect with a label attached, allowing multiple SQLite databases to be used in the same program.

Supported interpreters:

  * 'runSQLiteUnsync' - Runs a single-threaded action with a single connection.
  * 'runSQLiteSync' - Runs an action with a single connection shared across multiple threads.

>>> :set -XTypeApplications
>>> import Effectful
>>> import Effectful.Concurrent (runConcurrent)
>>> import Effectful.SQLite.Simple.Labeled (Labeled, SQLite)
>>> import Effectful.SQLite.Simple.Labeled qualified as SQL
>>> import Control.Concurrent.MVar qualified as MVar

>>> :{
app ::
  (Labeled "users" SQLite :> es)  =>
  (Labeled "products" SQLite :> es)  =>
  Eff es ()
app = do
  users <- SQL.useConnection @"users" \usersConn -> do
    SQL.query_ @User usersConn "SELECT * FROM users"
  products <- SQL.useConnection @"products" \productsConn -> do
    SQL.query_ @Product productsConn "SELECT * FROM products"
  pure ()
:}

>>> :{
main :: IO ()
main =
  SQL.withConnection "users.db" \usersConn -> do
    SQL.withConnection "products.db" \productsConn -> do
      usersConnVar <- MVar.newMVar usersConn
      productsConnVar <- MVar.newMVar productsConn
      app
        & SQL.runSQLiteSync @"users" usersConnVar
        & SQL.runSQLiteSync @"products" productsConnVar
        & runConcurrent
        & runEff
:}

-}
{- ORMOLU_ENABLE -}
module Effectful.SQLite.Simple.Labeled
  ( -- * Effects
    Labeled,
    SQLite (..),
    useConnection,
    LConnection (..),

    -- * Interpreters
    runSQLiteUnsync,
    runSQLiteSync,

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
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar (MVar)
import Effectful.Dispatch.Dynamic (send)
import Effectful.Dispatch.Static (seqUnliftIO, unsafeEff, unsafeEff_)
import Effectful.Labeled
import Effectful.SQLite.Simple (SQLite)
import Effectful.SQLite.Simple qualified as SQL
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

-- | A labeled connection.
newtype LConnection (label :: k) = LConnection {getConn :: S.Connection}

-- | Retrieve the connection from the context and run the given action with it.
--
-- __WARNING__:
--
-- * The connection must not escape the scope of `useConnection`.
-- * `useConnection` calls must not be nested.
-- * When used together with other locking primitives, the locks must always be acquired in the same order to avoid deadlocks.
--
-- E.g., in the example below, the connections to the 2 databases are acquired out of order, which could lead to a deadlock.
--
-- >>> :{
-- import Effectful
-- import Effectful.SQLite.Simple.Labeled (Labeled, SQLite)
-- import Effectful.SQLite.Simple.Labeled qualified as SQL
-- f, g :: (Labeled "users" SQLite :> es, Labeled "products" SQLite :> es) => Eff es ()
-- f = do
--   SQL.useConnection @"users" \usersConn -> do
--     SQL.useConnection @"products" \productsConn -> do
--       pure ()
-- g = do
--   SQL.useConnection @"products" \productsConn -> do
--    SQL.useConnection @"users" \usersConn -> do
--       pure ()
-- :}
useConnection :: forall label es a. (Labeled label SQLite :> es) => (LConnection label -> Eff es a) -> Eff es a
useConnection use = send $ Labeled @label $ SQL.UseConnection \conn -> use (LConnection conn)

----------------------------------------------------------------------------
-- Interpreters
----------------------------------------------------------------------------

-- | Runs a single-threaded action with a single connection.
runSQLiteUnsync ::
  forall label es a.
  (HasCallStack, IOE :> es) =>
  S.Connection -> Eff (Labeled label SQLite ': es) a -> Eff es a
runSQLiteUnsync = runLabeled @label . SQL.runSQLiteUnsync

-- | Runs an action with a single connection shared across multiple threads.
--
-- __WARNING__: Since this interpreter is backed by an `MVar`, the usual caveats apply:
--
-- * The connection must not escape the scope of `useConnection`.
-- * `useConnection` calls must not be nested.
-- * When used together with other locking primitives, the locks must always be acquired in the same order to avoid deadlocks.
runSQLiteSync ::
  forall label es a.
  (HasCallStack, IOE :> es, Concurrent :> es) =>
  MVar S.Connection -> Eff (Labeled label SQLite ': es) a -> Eff es a
runSQLiteSync = runLabeled @label . SQL.runSQLiteSync

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

query :: forall q r label es. (Labeled label SQLite :> es) => (ToRow q, FromRow r) => LConnection label -> Query -> q -> Eff es [r]
query conn q params = unsafeEff_ $ S.query conn.getConn q params

query_ :: forall r label es. (Labeled label SQLite :> es) => (FromRow r) => LConnection label -> Query -> Eff es [r]
query_ conn q = unsafeEff_ $ S.query_ conn.getConn q

queryWith :: forall q r label es. (Labeled label SQLite :> es) => (ToRow q) => RowParser r -> LConnection label -> Query -> q -> Eff es [r]
queryWith parser conn q params = unsafeEff_ $ S.queryWith parser conn.getConn q params

queryWith_ :: forall r label es. (Labeled label SQLite :> es) => RowParser r -> LConnection label -> Query -> Eff es [r]
queryWith_ parser conn q = unsafeEff_ $ S.queryWith_ parser conn.getConn q

queryNamed :: forall r label es. (Labeled label SQLite :> es) => (FromRow r) => LConnection label -> Query -> [NamedParam] -> Eff es [r]
queryNamed conn q params = unsafeEff_ $ S.queryNamed conn.getConn q params

lastInsertRowId :: forall label es. (Labeled label SQLite :> es) => LConnection label -> Eff es Int64
lastInsertRowId = unsafeEff_ . S.lastInsertRowId . getConn

changes :: forall label es. (Labeled label SQLite :> es) => LConnection label -> Eff es Int
changes = unsafeEff_ . S.changes . getConn

totalChanges :: forall label es. (Labeled label SQLite :> es) => LConnection label -> Eff es Int
totalChanges = unsafeEff_ . S.totalChanges . getConn

fold :: forall row params a label es. (Labeled label SQLite :> es, FromRow row, ToRow params) => LConnection label -> Query -> params -> a -> (a -> row -> Eff es a) -> Eff es a
fold conn q params initialState action =
  unsafeEffWithUnlift @label \unlift -> do
    S.fold conn.getConn q params initialState \a row ->
      unlift $ action a row

fold_ :: forall row a label es. (Labeled label SQLite :> es) => (FromRow row) => LConnection label -> Query -> a -> (a -> row -> Eff es a) -> Eff es a
fold_ conn q initialState action =
  unsafeEffWithUnlift @label \unlift -> do
    S.fold_ conn.getConn q initialState \a row ->
      unlift $ action a row

foldNamed :: forall row a label es. (Labeled label SQLite :> es) => (FromRow row) => LConnection label -> Query -> [NamedParam] -> a -> (a -> row -> Eff es a) -> Eff es a
foldNamed conn q params initialState action =
  unsafeEffWithUnlift @label \unlift -> do
    S.foldNamed conn.getConn q params initialState \a row ->
      unlift $ action a row

execute :: forall q label es. (Labeled label SQLite :> es) => (ToRow q) => LConnection label -> Query -> q -> Eff es ()
execute conn q params = unsafeEff_ $ S.execute conn.getConn q params

execute_ :: forall label es. (Labeled label SQLite :> es) => LConnection label -> Query -> Eff es ()
execute_ conn q = unsafeEff_ $ S.execute_ conn.getConn q

executeMany :: forall q label es. (Labeled label SQLite :> es) => (ToRow q) => LConnection label -> Query -> [q] -> Eff es ()
executeMany conn q params = unsafeEff_ $ S.executeMany conn.getConn q params

executeNamed :: forall label es. (Labeled label SQLite :> es) => LConnection label -> Query -> [NamedParam] -> Eff es ()
executeNamed conn q params = unsafeEff_ $ S.executeNamed conn.getConn q params

withTransaction :: forall a label es. (Labeled label SQLite :> es) => LConnection label -> Eff es a -> Eff es a
withTransaction conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withTransaction conn.getConn $ unlift action

withImmediateTransaction :: forall a label es. (Labeled label SQLite :> es) => LConnection label -> Eff es a -> Eff es a
withImmediateTransaction conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withImmediateTransaction conn.getConn $ unlift action

withExclusiveTransaction :: forall a label es. (Labeled label SQLite :> es) => LConnection label -> Eff es a -> Eff es a
withExclusiveTransaction conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withExclusiveTransaction conn.getConn $ unlift action

withSavepoint :: forall a label es. (Labeled label SQLite :> es) => LConnection label -> Eff es a -> Eff es a
withSavepoint conn action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withSavepoint conn.getConn $ unlift action

openStatement :: forall label es. (Labeled label SQLite :> es) => LConnection label -> Query -> Eff es Statement
openStatement conn q = unsafeEff_ $ S.openStatement conn.getConn q

closeStatement :: forall label es. (Labeled label SQLite :> es) => Statement -> Eff es ()
closeStatement stmt = unsafeEff_ $ S.closeStatement stmt

withStatement :: forall a label es. (Labeled label SQLite :> es) => LConnection label -> Query -> (Statement -> Eff es a) -> Eff es a
withStatement conn q action =
  unsafeEffWithUnlift @label \unlift -> do
    S.withStatement conn.getConn q (unlift . action)

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
