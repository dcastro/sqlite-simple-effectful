{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar qualified as MVar
import Control.Monad (forever)
import Data.Function ((&))
import Database.SQLite.Simple (Only)
import Database.SQLite.Simple qualified as SS
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Labeled (Labeled)
import Effectful.SQLite.Simple qualified as S
import Effectful.SQLite.Simple.Labeled qualified as L
import Effectful.SQLite.Simple.RW qualified as RW
import GHC.Conc qualified as Conc
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Temp qualified as Temp

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("1" : _) -> example1
    ("2" : _) -> example2
    ("3" : _) -> example3
    ("4" : _) -> example4
    ("5" : _) -> example5
    [] -> die "Missing argument"
    args -> die $ "Invalid migration number, args:" <> show args

example1 :: IO ()
example1 = do
  c <- SS.open ":memory:"
  mv <- MVar.newMVar c
  t1 & S.runSQLiteSync mv & runConcurrent & runEff
  where
    t1 :: (S.SQLite :> es, IOE :> es) => Eff es ()
    t1 = do
      S.useConnection \conn -> do
        res <- S.query_ @_ @(Only Int) conn "SELECT 1 + 3"
        liftIO $ print res
        pure ()

{-

This always deadlocks and crashes with:

sqlite-simple-effectful-examples: Uncaught exception ghc-internal:GHC.Internal.IO.Exception.BlockedIndefinitelyOnMVar:
thread blocked indefinitely in an MVar operation
While handling thread blocked indefinitely in an STM transaction

 -}
example2 :: IO ()
example2 = do
  cx <- SS.open ":memory:"
  cy <- SS.open ":memory:"
  mvx <- MVar.newMVar cx
  mvy <- MVar.newMVar cy
  handlexy <- Async.async $ xy & L.runSQLiteSync @"x" mvx & L.runSQLiteSync @"y" mvy & runConcurrent & runEff
  handleyx <- Async.async $ yx & L.runSQLiteSync @"x" mvx & L.runSQLiteSync @"y" mvy & runConcurrent & runEff
  Async.wait handlexy >> Async.wait handleyx

-- | Acquires the "x" connection and then the "y" connection.
xy :: (Labeled "x" S.SQLite :> es, Labeled "y" S.SQLite :> es, Concurrent :> es) => Eff es ()
xy = do
  L.useConnection @"x" \_conn1 -> do
    threadDelay 1_e6
    L.useConnection @"y" \_conn2 -> do
      pure ()

-- | Acquires the "y" connection and then the "x" connection.
yx :: (Labeled "x" S.SQLite :> es, Labeled "y" S.SQLite :> es, Concurrent :> es) => Eff es ()
yx = do
  L.useConnection @"y" \_conn1 -> do
    threadDelay 1_e6
    L.useConnection @"x" \_conn2 -> do
      pure ()

{-
This test starts 2 connections sequentially, and then starts 2 threads that use those connections to read/write.
Even with WAL enabled, this can lead to a deadlock.

This test will invariably fail with:
> SQLite3 returned ErrorBusy while attempting to perform prepare "INSERT INTO test (value) VALUES ('Hello, world!')": database is locked

See: https://sqlite.org/wal.html#sometimes_queries_return_sqlite_busy_in_wal_mode
-}

example3 :: IO ()
example3 = do
  forever do
    Temp.withSystemTempFile "sqlite-simple-effectful-example.db" \dbPath _dbHandle -> do
      SS.withConnection dbPath \conn -> do
        SS.execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, value TEXT)"

      writeConn <- SS.open dbPath
      readConn <- SS.open dbPath

      handleReader2 <- Async.async $ reader readConn "2"
      handleWriter <- Async.async $ writer writeConn "1"

      Async.wait handleWriter
      Async.wait handleReader2
  where
    reader :: SS.Connection -> String -> IO ()
    reader conn label = do
      [SS.Only count] <- SS.query_ @(Only Int) conn "SELECT COUNT(*) FROM test"
      liftIO $ putStrLn $ "Read from " <> label <> ": " <> show count
    writer :: SS.Connection -> String -> IO ()
    writer conn label = do
      SS.execute conn "INSERT INTO test (value) VALUES ('Hello, world!')" ()
      liftIO $ putStrLn $ "Inserted by " <> label

{-
Even if we modify the example above to be single thread, the test will still fail,
albeit less frequently and with a different error:

> SQLite3 returned ErrorCan'tOpen while attempting to perform prepare "INSERT INTO test (value) VALUES ('Hello, world!')": unable to open database file
 -}
example4 :: IO ()
example4 = do
  forever do
    Temp.withSystemTempFile "sqlite-simple-effectful-example.db" \dbPath _dbHandle -> do
      SS.withConnection dbPath \conn -> do
        SS.execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, value TEXT)"

      writeConn <- SS.open dbPath
      readConn <- SS.open dbPath
      [SS.Only count] <- SS.query_ @(Only Int) readConn "SELECT COUNT(*) FROM test"
      liftIO $ putStrLn $ "Read from " <> "1" <> ": " <> show count
      SS.execute writeConn "INSERT INTO test (value) VALUES ('Hello, world!')" ()
      liftIO $ putStrLn $ "Inserted by " <> "1"

{-
Adding some delay, so that the threads don't start their connections all at once,
alleviates the problem.
-}
example5 :: IO ()
example5 = do
  Temp.withSystemTempFile "sqlite-simple-effectful-example.db" \dbPath _dbHandle -> do
    SS.withConnection dbPath \conn -> do
      SS.execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, value TEXT)"

    pools <-
      RW.newPools
        =<< RW.newPoolsConfig
          (SS.open dbPath)
          10
          10
          (SS.open dbPath)
          10

    -- Add some delay, so that the threads don't start their connections all at once.
    -- Starting many connections at once can lead to a `database is locked` error, even in WAL mode.
    handleReader1 <- Async.async $ reader "1" & RW.runSQLiteWithPools pools & runEff
    Conc.threadDelay 500_000
    handleReader2 <- Async.async $ reader "2" & RW.runSQLiteWithPools pools & runEff
    Conc.threadDelay 500_000
    handleWriter <- Async.async $ writer "1" & RW.runSQLiteWithPools pools & runEff

    Async.wait handleReader1
    Async.wait handleReader2
    Async.wait handleWriter
  where
    reader2 :: (S.SQLite :> es, IOE :> es) => String -> Eff es ()
    reader2 label = do
      sc <- S.useConnection \conn -> do
        res <- S.query_ @_ @(Only Int) conn "SELECT COUNT(*) FROM test"
        liftIO $ putStrLn $ "Read from " <> label <> ": " <> show res
        pure $ SomeConn conn

      case sc of
        SomeConn conn -> do
          res <- S.query_ @_ @(Only Int) conn "SELECT COUNT(*) FROM test"
          liftIO $ putStrLn $ "Read from " <> label <> ": " <> show res
      reader2 label
    reader :: (RW.SQLite :> es, IOE :> es) => String -> Eff es ()
    reader label = do
      RW.useReadConnection \conn -> do
        res <- RW.query_ @_ @(Only Int) conn "SELECT COUNT(*) FROM test"
        liftIO $ putStrLn $ "Read from " <> label <> ": " <> show res
      reader label
    writer :: (RW.SQLite :> es, IOE :> es) => String -> Eff es ()
    writer label = do
      RW.useWriteConnection \conn -> do
        RW.execute conn "INSERT INTO test (value) VALUES ('Hello, world!')" ()
        liftIO $ putStrLn $ "Inserted by " <> label
      writer label

data SomeConn where
  SomeConn :: S.SConnection s -> SomeConn
