

## Implicit connection

I considered using an approach similar to `effectful-postgresql`.
By default, the user is meant to use `withConnection`, and then pass the connection to the raw functions from `postgresql-simple`.
But it also has a more convenient mode, where the user:

* Does not have to use `withConnection`
* Use the functions from `Effectful.PostgreSQL` (query, execute, etc) which internally call `withConnection` individually.

There are some issues with this 2nd mode:
* When running multiple sqlite statements with a "pooled" interpreter, this design would imply repeatedly acquiring and releasing a connection from the pool, unnecessarily.
* This design would not allow running the effect by creating a single connection that is guarded by an `MVar`. E.g.:
    * Here, you could argue that `query` acquires the MVar, performs the query, then releases it:
      ```hs11
      res <- query "SELECT ..."
      ```
    * But what would happen when transactions are thrown into the mix?
      If `withTransactions` holds the lock for the entire duration of the transaction, and then `query` also tries to acquire it, we'll have a deadlock.
      ```hs
      withTransaction do
        res <- query "SELECT ..."
      ```
* Transactions and their operations can potentially be run with different connections:
  In the example below, `withTransaction` and `query` will both fetch a connection from the pool, and end up getting potentially different connections.
    ```hs
    withTransaction do
      res <- query "SELECT ..."
    ```

Instead, I opted to:

* have every function require a connection to be explicitly passed in.
* require the user to manually call `withConnection`, and warn the user not to nest these calls when using an implemented backed by `MVar`


## `IOE`

In `effectful-postgresql`, every function requires `IOE`.
To avoid this, we internally used `unsafeEff`; this is actually safe, as long as all the `run*` operations require `IOE`.

## SQLite :> es

The db operations (see example below) do not really require the `SQLite es` effect.

I decided to keep it anyway.
If they didn't have any effect attached, they could be run with `runPureEff`,
which would fail because they internally use `unsafeEff_` and thus can only be run with `runEff`.

```hs
lastInsertRowId :: (SQLite :> es) => Connection -> Eff es Int64
lastInsertRowId = unsafeEff_ . S.lastInsertRowId
```
