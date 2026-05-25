{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar qualified as MVar
import Data.Function ((&))
import Database.SQLite.Simple (Only)
import Database.SQLite.Simple qualified as SS
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Labeled (Labeled)
import Effectful.SQLite.Simple.Internal qualified as S
import Effectful.SQLite.Simple.Internal.Labeled qualified as L
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case drop 1 args of
    ("1" : _) -> example1
    ("2" : _) -> example2
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
      S.withConnection \conn -> do
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
  L.withConnection @"x" \_conn1 -> do
    threadDelay 1_e6
    L.withConnection @"y" \_conn2 -> do
      pure ()

-- | Acquires the "y" connection and then the "x" connection.
yx :: (Labeled "x" S.SQLite :> es, Labeled "y" S.SQLite :> es, Concurrent :> es) => Eff es ()
yx = do
  L.withConnection @"y" \_conn1 -> do
    threadDelay 1_e6
    L.withConnection @"x" \_conn2 -> do
      pure ()
