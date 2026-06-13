# Conventions

* I made liberal use of `unsafeEff`, which means we must ensure all `run*` interpreters have the (possibly redundant) constraint `IOE :> es`.
* All interpreters and operations that use `send` must have `HasCallStack`.
* All `Internal` modules must have `{-# OPTIONS_HADDOCK not-home #-}`
* Explicit type arguments:
  * All db operations must have an explicit `forall`
  * The `es` type arg must always come last, to ensure consistency with the `sqlite-simple` package (e.g. in both packages, the first type arg of `query` should be `q`)
    ```hs
    query :: forall q r es.
    ```
  * In the `Labeled` modules:
    * If a function takes a `Connection label`, then the `label` type arg will be inferred by GHC, so we can push it to the end of the list.
      E.g.
      ```hs
      query :: forall q r label es.
      query :: forall q r label mode es.
      ```
    * Otherwise, the `label` type arg should come first to make it easy to use with `TypeApplications`.
      ```hs
      withBind :: forall label params a es.
      ```
