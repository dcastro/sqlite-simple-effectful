# Conventions

* All functions from the `Labeled` modules must ensure `label` is the first type parameter, to make it easy to use with `TypeApplications`.
* I made liberal use of `unsafeEff`, which means we must ensure all `run*` interpreters have the (possibly redundant) constraint `IOE :> es`.
* All interpreters and operations that use `send` must have `HasCallStack`.
* All `Internal` modules must have `{-# OPTIONS_HADDOCK not-home #-}`
