{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK not-home #-}

module Effectful.SQLite.Simple.Internal where

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
  UseConnection :: (Connection -> m a) -> SQLite m a

type instance DispatchOf SQLite = 'Dynamic

useConnection :: (HasCallStack, SQLite :> es) => (Connection -> Eff es a) -> Eff es a
useConnection = send . UseConnection

----------------------------------------------------------------------------
-- Interpreters
----------------------------------------------------------------------------

runSQLiteUnsync ::
  (HasCallStack, IOE :> es) =>
  Connection -> Eff (SQLite ': es) a -> Eff es a
runSQLiteUnsync conn =
  interpret \env -> \case
    UseConnection f ->
      localSeqUnlift env \unlift -> unlift $ f conn

runSQLiteSync ::
  (HasCallStack, IOE :> es, Concurrent :> es) =>
  MVar Connection -> Eff (SQLite ': es) a -> Eff es a
runSQLiteSync connVar = do
  interpret \env -> \case
    UseConnection f ->
      localSeqUnlift env \unlift -> do
        MVar.withMVar connVar \conn -> do
          unlift $ f conn

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

query :: (SQLite :> es) => (ToRow q, FromRow r) => Connection -> Query -> q -> Eff es [r]
query conn q params = unsafeEff_ $ S.query conn q params

query_ :: (SQLite :> es) => (FromRow r) => Connection -> Query -> Eff es [r]
query_ conn q = unsafeEff_ $ S.query_ conn q

queryWith :: (SQLite :> es) => (ToRow q) => RowParser r -> Connection -> Query -> q -> Eff es [r]
queryWith parser conn q params = unsafeEff_ $ S.queryWith parser conn q params

queryWith_ :: (SQLite :> es) => RowParser r -> Connection -> Query -> Eff es [r]
queryWith_ parser conn q = unsafeEff_ $ S.queryWith_ parser conn q

queryNamed :: (SQLite :> es) => (FromRow r) => Connection -> Query -> [NamedParam] -> Eff es [r]
queryNamed conn q params = unsafeEff_ $ S.queryNamed conn q params

lastInsertRowId :: (SQLite :> es) => Connection -> Eff es Int64
lastInsertRowId = unsafeEff_ . S.lastInsertRowId

changes :: (SQLite :> es) => Connection -> Eff es Int
changes = unsafeEff_ . S.changes

totalChanges :: (SQLite :> es) => Connection -> Eff es Int
totalChanges = unsafeEff_ . S.totalChanges

fold :: (SQLite :> es) => (FromRow row, ToRow params) => Connection -> Query -> params -> a -> (a -> row -> Eff es a) -> Eff es a
fold conn q params initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.fold conn q params initialState \a row ->
      unlift $ action a row

fold_ :: (SQLite :> es) => (FromRow row) => Connection -> Query -> a -> (a -> row -> Eff es a) -> Eff es a
fold_ conn q initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.fold_ conn q initialState \a row ->
      unlift $ action a row

foldNamed :: (SQLite :> es) => (FromRow row) => Connection -> Query -> [NamedParam] -> a -> (a -> row -> Eff es a) -> Eff es a
foldNamed conn q params initialState action =
  unsafeEffWithUnlift \unlift -> do
    S.foldNamed conn q params initialState \a row ->
      unlift $ action a row

execute :: (SQLite :> es) => (ToRow q) => Connection -> Query -> q -> Eff es ()
execute conn q params = unsafeEff_ $ S.execute conn q params

execute_ :: (SQLite :> es) => Connection -> Query -> Eff es ()
execute_ conn q = unsafeEff_ $ S.execute_ conn q

executeMany :: (SQLite :> es) => (ToRow q) => Connection -> Query -> [q] -> Eff es ()
executeMany conn q params = unsafeEff_ $ S.executeMany conn q params

executeNamed :: (SQLite :> es) => Connection -> Query -> [NamedParam] -> Eff es ()
executeNamed conn q params = unsafeEff_ $ S.executeNamed conn q params

withTransaction :: (SQLite :> es) => Connection -> Eff es a -> Eff es a
withTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withTransaction conn $ unlift action

withImmediateTransaction :: (SQLite :> es) => Connection -> Eff es a -> Eff es a
withImmediateTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withImmediateTransaction conn $ unlift action

withExclusiveTransaction :: (SQLite :> es) => Connection -> Eff es a -> Eff es a
withExclusiveTransaction conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withExclusiveTransaction conn $ unlift action

withSavepoint :: (SQLite :> es) => Connection -> Eff es a -> Eff es a
withSavepoint conn action =
  unsafeEffWithUnlift \unlift -> do
    S.withSavepoint conn $ unlift action

openStatement :: (SQLite :> es) => Connection -> Query -> Eff es Statement
openStatement conn q = unsafeEff_ $ S.openStatement conn q

closeStatement :: (SQLite :> es) => Statement -> Eff es ()
closeStatement stmt = unsafeEff_ $ S.closeStatement stmt

withStatement :: (SQLite :> es) => Connection -> Query -> (Statement -> Eff es a) -> Eff es a
withStatement conn q action =
  unsafeEffWithUnlift \unlift -> do
    S.withStatement conn q (unlift . action)

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
