{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.SQLite.Simple
  ( -- * Effects
    SQLite (..),
    useConnection,
    SConnection (..),

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
import Database.SQLite.Simple (ColumnIndex, Connection, FromRow, NamedParam, Query, Statement, ToRow)
import Database.SQLite.Simple qualified as S
import Database.SQLite.Simple.FromRow (RowParser)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.MVar (MVar)
import Effectful.Concurrent.MVar qualified as MVar
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, send)
import Effectful.Dispatch.Static (seqUnliftIO, unsafeEff, unsafeEff_)
import GHC.Stack (HasCallStack)

----------------------------------------------------------------------------
-- Effect
----------------------------------------------------------------------------

data SQLite :: Effect where
  UseConnection :: (SConnection s -> m a) -> SQLite m a

type instance DispatchOf SQLite = 'Dynamic

useConnection :: (HasCallStack, SQLite :> es) => (forall s. SConnection s -> Eff es a) -> Eff es a
useConnection use = send $ UseConnection use

-- | A "scoped connection" that can only be used in the scope of `useConnection`.
newtype SConnection (s :: k) = SConnection {getConn :: S.Connection}

----------------------------------------------------------------------------
-- Interpreters
----------------------------------------------------------------------------

runSQLiteUnsync ::
  (HasCallStack, IOE :> es) =>
  Connection -> Eff (SQLite ': es) a -> Eff es a
runSQLiteUnsync conn =
  interpret \env -> \case
    UseConnection f ->
      localSeqUnlift env \unlift -> unlift $ f (SConnection conn)

runSQLiteSync ::
  (HasCallStack, IOE :> es, Concurrent :> es) =>
  MVar Connection -> Eff (SQLite ': es) a -> Eff es a
runSQLiteSync connVar = do
  interpret \env -> \case
    UseConnection f ->
      localSeqUnlift env \unlift -> do
        MVar.withMVar connVar \conn -> do
          unlift $ f (SConnection conn)

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

{-
Notes:
  * We're using `unsafeEff_` to avoid having `IOE` show up in every type sig.
    This is fine, _as long as_:
      * we ensure all the `run*` functions require `IOE`.
      * All operations must have the `SQLite :> es` constraint (even if GHC considers it redundant),
        to ensure they cannot be run with `runPureEff` and bypass the `IOE` requirement.

-}

query :: (SQLite :> es) => (ToRow q, FromRow r) => SConnection s -> Query -> q -> Eff es [r]
query conn q params = unsafeEff_ $ S.query conn.getConn q params

query_ :: (SQLite :> es) => (FromRow r) => SConnection s -> Query -> Eff es [r]
query_ conn q = unsafeEff_ $ S.query_ conn.getConn q

queryWith :: (SQLite :> es) => (ToRow q) => RowParser r -> SConnection s -> Query -> q -> Eff es [r]
queryWith parser conn q params = unsafeEff_ $ S.queryWith parser conn.getConn q params

queryWith_ :: (SQLite :> es) => RowParser r -> SConnection s -> Query -> Eff es [r]
queryWith_ parser conn q = unsafeEff_ $ S.queryWith_ parser conn.getConn q

queryNamed :: (SQLite :> es) => (FromRow r) => SConnection s -> Query -> [NamedParam] -> Eff es [r]
queryNamed conn q params = unsafeEff_ $ S.queryNamed conn.getConn q params

lastInsertRowId :: (SQLite :> es) => SConnection s -> Eff es Int64
lastInsertRowId = unsafeEff_ . S.lastInsertRowId . getConn

changes :: (SQLite :> es) => SConnection s -> Eff es Int
changes = unsafeEff_ . S.changes . getConn

totalChanges :: (SQLite :> es) => SConnection s -> Eff es Int
totalChanges = unsafeEff_ . S.totalChanges . getConn

fold :: (SQLite :> es) => (FromRow row, ToRow params) => SConnection s -> Query -> params -> a -> (a -> row -> Eff es a) -> Eff es a
fold conn q params initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.fold conn.getConn q params initialState \a row ->
      unlift $ action a row

fold_ :: (SQLite :> es) => (FromRow row) => SConnection s -> Query -> a -> (a -> row -> Eff es a) -> Eff es a
fold_ conn q initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.fold_ conn.getConn q initialState \a row ->
      unlift $ action a row

foldNamed :: (SQLite :> es) => (FromRow row) => SConnection s -> Query -> [NamedParam] -> a -> (a -> row -> Eff es a) -> Eff es a
foldNamed conn q params initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.foldNamed conn.getConn q params initialState \a row ->
      unlift $ action a row

execute :: (SQLite :> es) => (ToRow q) => SConnection s -> Query -> q -> Eff es ()
execute conn q params = unsafeEff_ $ S.execute conn.getConn q params

execute_ :: (SQLite :> es) => SConnection s -> Query -> Eff es ()
execute_ conn q = unsafeEff_ $ S.execute_ conn.getConn q

executeMany :: (SQLite :> es) => (ToRow q) => SConnection s -> Query -> [q] -> Eff es ()
executeMany conn q params = unsafeEff_ $ S.executeMany conn.getConn q params

executeNamed :: (SQLite :> es) => SConnection s -> Query -> [NamedParam] -> Eff es ()
executeNamed conn q params = unsafeEff_ $ S.executeNamed conn.getConn q params

withTransaction :: (SQLite :> es) => SConnection s -> Eff es a -> Eff es a
withTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withTransaction conn.getConn $ unlift action

withImmediateTransaction :: (SQLite :> es) => SConnection s -> Eff es a -> Eff es a
withImmediateTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withImmediateTransaction conn.getConn $ unlift action

withExclusiveTransaction :: (SQLite :> es) => SConnection s -> Eff es a -> Eff es a
withExclusiveTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withExclusiveTransaction conn.getConn $ unlift action

withSavepoint :: (SQLite :> es) => SConnection s -> Eff es a -> Eff es a
withSavepoint conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withSavepoint conn.getConn $ unlift action

openStatement :: (SQLite :> es) => SConnection s -> Query -> Eff es Statement
openStatement conn q = unsafeEff_ $ S.openStatement conn.getConn q

closeStatement :: (SQLite :> es) => Statement -> Eff es ()
closeStatement stmt = unsafeEff_ $ S.closeStatement stmt

withStatement :: (SQLite :> es) => SConnection s -> Query -> (Statement -> Eff es a) -> Eff es a
withStatement conn q action =
  unsafeEffWithUnlift \unlift -> do
    S.withStatement conn.getConn q (unlift . action)

bind :: (SQLite :> es) => (ToRow params) => Statement -> params -> Eff es ()
bind stmt params = unsafeEff_ $ S.bind stmt params

bindNamed :: (SQLite :> es) => Statement -> [NamedParam] -> Eff es ()
bindNamed stmt params = unsafeEff_ $ S.bindNamed stmt params

reset :: (SQLite :> es) => Statement -> Eff es ()
reset stmt = unsafeEff_ $ S.reset stmt

columnName :: (SQLite :> es) => Statement -> ColumnIndex -> Eff es Text
columnName stmt idx = unsafeEff_ $ S.columnName stmt idx

columnCount :: (SQLite :> es) => Statement -> Eff es ColumnIndex
columnCount stmt = unsafeEff_ $ S.columnCount stmt

withBind :: (SQLite :> es) => (ToRow params) => Statement -> params -> Eff es a -> Eff es a
withBind stmt params action =
  unsafeEffWithUnlift \unlift -> do
    S.withBind stmt params $ unlift action

nextRow :: (SQLite :> es) => (FromRow r) => Statement -> Eff es (Maybe r)
nextRow stmt = unsafeEff_ $ S.nextRow stmt

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

unsafeEffWithUnlift :: forall a es. (SQLite :> es) => ((forall x. Eff es x -> IO x) -> IO a) -> Eff es a
unsafeEffWithUnlift action =
  unsafeEff \env -> do
    seqUnliftIO env \unlift -> do
      action unlift
