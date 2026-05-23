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
    This is fine, _as long as_ we ensure all the `run*` functions require `IOE`.

-}
data SQLite :: Effect where
  WithConnection :: (Connection -> m a) -> SQLite m a

type instance DispatchOf SQLite = 'Dynamic

withConnection :: (SQLite :> es) => (Connection -> Eff es a) -> Eff es a
withConnection = send . WithConnection

query :: (SQLite :> es) => (ToRow q, FromRow r) => Query -> q -> Eff es [r]
query q params = withConnection \conn -> unsafeEff_ $ S.query conn q params

query_ :: (SQLite :> es) => (FromRow r) => Query -> Eff es [r]
query_ q = withConnection \conn -> unsafeEff_ $ S.query_ conn q

queryWith :: (SQLite :> es) => (ToRow q) => RowParser r -> Query -> q -> Eff es [r]
queryWith parser q params = withConnection \conn -> unsafeEff_ $ S.queryWith parser conn q params

queryWith_ :: (SQLite :> es) => RowParser r -> Query -> Eff es [r]
queryWith_ parser q = withConnection \conn -> unsafeEff_ $ S.queryWith_ parser conn q

queryNamed :: (SQLite :> es) => (FromRow r) => Query -> [NamedParam] -> Eff es [r]
queryNamed q params = withConnection \conn -> unsafeEff_ $ S.queryNamed conn q params

lastInsertRowId :: (SQLite :> es) => Eff es Int64
lastInsertRowId = withConnection $ unsafeEff_ . S.lastInsertRowId

changes :: (SQLite :> es) => Eff es Int
changes = withConnection $ unsafeEff_ . S.changes

totalChanges :: (SQLite :> es) => Eff es Int
totalChanges = withConnection $ unsafeEff_ . S.totalChanges

fold :: forall es row params a. (SQLite :> es) => (FromRow row, ToRow params) => Query -> params -> a -> (a -> row -> Eff es a) -> Eff es a
fold q params initialState action = withConnection \conn -> do
  unsafeEff \env -> do
    seqUnliftIO env \unlift -> do
      S.fold conn q params initialState \a row ->
        unlift $ action a row

fold_ :: (SQLite :> es) => (FromRow row) => Query -> a -> (a -> row -> Eff es a) -> Eff es a
fold_ q initialState action =
  withConnection \conn -> do
    unsafeEff \env -> do
      seqUnliftIO env \unlift -> do
        S.fold_ conn q initialState \a row ->
          unlift $ action a row

foldNamed :: (SQLite :> es) => (FromRow row) => Query -> [NamedParam] -> a -> (a -> row -> Eff es a) -> Eff es a
foldNamed q params initialState action =
  withConnection \conn -> do
    unsafeEff \env -> do
      seqUnliftIO env \unlift -> do
        S.foldNamed conn q params initialState \a row ->
          unlift $ action a row

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
