{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.SQLite.Simple.Internal where

import Data.Int (Int64)
import Database.SQLite.Simple (Connection, FromRow, NamedParam, Query, ToRow)
import Database.SQLite.Simple qualified as S
import Database.SQLite.Simple.FromRow (RowParser)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import Effectful.Dispatch.Static (seqUnliftIO, unsafeEff, unsafeEff_)

{-
Notes:
  * We're using `unsafeEff_` to avoid having `IOE` show up in every type sig.
    This is fine, _as long as_:
      * we ensure all the `run*` functions require `IOE`.
      * All operations must have the `SQLite :> es` constraint (even if GHC considers it redundant),
        to ensure they cannot be run with `runPureEff` and bypass the `IOE` requirement.

-}
data SQLite :: Effect where
  WithConnection :: (Connection -> m a) -> SQLite m a

type instance DispatchOf SQLite = 'Dynamic

withConnection :: (SQLite :> es) => (Connection -> Eff es a) -> Eff es a
withConnection = send . WithConnection

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
  unsafeEff \env -> do
    seqUnliftIO env \unlift -> do
      S.fold conn q params initialState \a row ->
        unlift $ action a row

fold_ :: (SQLite :> es) => (FromRow row) => Connection -> Query -> a -> (a -> row -> Eff es a) -> Eff es a
fold_ conn q initialState action =
  unsafeEff \env -> do
    seqUnliftIO env \unlift -> do
      S.fold_ conn q initialState \a row ->
        unlift $ action a row

foldNamed :: (SQLite :> es) => (FromRow row) => Connection -> Query -> [NamedParam] -> a -> (a -> row -> Eff es a) -> Eff es a
foldNamed conn q params initialState action =
  unsafeEff \env -> do
    seqUnliftIO env \unlift -> do
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

{-

query_ :: FromRow r => Connection -> Query -> IO [r]
queryWith :: ToRow q => RowParser r -> Connection -> Query -> q -> IO [r]
queryWith_ :: RowParser r -> Connection -> Query -> IO [r]
queryNamed :: FromRow r => Connection -> Query -> [NamedParam] -> IO [r]
lastInsertRowId :: Connection -> IO Int64
changes :: Connection -> IO Int
totalChanges :: Connection -> IO Int

fold :: (FromRow row, ToRow params) => Connection -> Query -> params -> a -> (a -> row -> IO a) -> IO a
fold_ :: FromRow row => Connection -> Query -> a -> (a -> row -> IO a) -> IO a
foldNamed :: FromRow row => Connection -> Query -> [NamedParam] -> a -> (a -> row -> IO a) -> IO a

execute :: ToRow q => Connection -> Query -> q -> IO ()
execute_ :: Connection -> Query -> IO ()
executeMany :: ToRow q => Connection -> Query -> [q] -> IO ()
executeNamed :: Connection -> Query -> [NamedParam] -> IO ()

withTransaction :: Connection -> IO a -> IO a
withImmediateTransaction :: Connection -> IO a -> IO a
withExclusiveTransaction :: Connection -> IO a -> IO a
withSavepoint :: Connection -> IO a -> IO a
openStatement :: Connection -> Query -> IO Statement
closeStatement :: Statement -> IO ()
withStatement :: Connection -> Query -> (Statement -> IO a) -> IO a
bind :: ToRow params => Statement -> params -> IO ()
bindNamed :: Statement -> [NamedParam] -> IO ()
reset :: Statement -> IO ()
columnName :: Statement -> ColumnIndex -> IO Text
columnCount :: Statement -> IO ColumnIndex
withBind :: ToRow params => Statement -> params -> IO a -> IO a
nextRow :: FromRow r => Statement -> IO (Maybe r)
 -}
