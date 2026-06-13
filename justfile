# Just list all recipes by default
default:
    just --list

checks:
    just doctest
    just haddock
    just pandoc
    # check markdown links
    xrefcheck --ignore "release/**/*"
    # Check cross-references "ref:" in the repo
    xreferee --include-untracked
    # Build with `-Werror`
    stack clean && stack build --fast --test --bench --no-run-tests --no-run-benchmarks --ghc-options "-Werror"
    # Build with the lowest supported version of each dependency.
    cabal clean && just min-deps

# Build the project with the lowest supported version of each dependency.
min-deps:
    cabal build all \
        --constraint='effectful ==2.4.0.0' \
        --constraint='effectful-core ==2.4.0.0' \
        --constraint='sqlite-simple ==0.4.19.0' \
        --constraint='unliftio-pool ==0.4.2.0' \
        --constraint='async ==2.2.5' \
        --constraint='temporary ==1.3' \
        --ghc-options="-Werror" \
        --with-compiler=ghc-9.10.3

doctest:
    ./scripts/check_doctest.sh
    stack build doctest
    stack exec doctest -- $(find src test \( -name '*.lhs' -o -name '*.hs' \) -print) \
        -XGHC2024 -XBlockArguments -XTypeFamilies -XQualifiedDo

haddock:
    ./scripts/check_haddock_warnings.sh lib:sqlite-simple-effectful

# Run haddock in "file watch" mode
haddock-fw:
    watchexec --clear --exts hs -- just haddock

haddock-hackage *ARGS:
    cabal update
    cabal haddock lib:sqlite-simple-effectful --haddock-for-hackage {{ ARGS }}

pandoc:
    ./scripts/run_pandoc.sh

############################################################################
## Release
############################################################################
# Checklist:
# - [ ] Update version in `package.yaml`
# - [ ] Update changelog
# - [ ] Add `@since` annotations to all new public API
# - [ ] Review the `min-deps` command
# - [ ] Create GitHub release & tag the commit

publish-candidate:
    just checks

    rm -rf dist-newstyle
    rm -rf release && mkdir release

    cabal sdist --builddir release
    cabal upload release/sdist/*.tar.gz

publish-candidate-docs *ARGS:
    just checks

    rm -rf release/docs
    mkdir -p release/docs
    cabal update
    cabal haddock lib:sqlite-simple-effectful --haddock-for-hackage --builddir release/docs
    cabal upload --documentation {{ ARGS }} release/docs/*-docs.tar.gz

publish-final-docs:
    just publish-candidate-docs --publish
