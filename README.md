![Haskell CI](https://github.com/andreabedini/ministg/workflows/Haskell%20CI/badge.svg)

Thank you for trying Ministg. Check out the [Documentation].

Install with `cabal install` from the root directory. Or `cabal install
ministg` from anywhere to install from hackage.

## Feedback, feature requests or bug reports

Bernie Pope: <http://www.berniepope.id.au>, alternatively issues on github.

## Usage

```
cabal run ministg -- test/Prelude.stg test/sum.stg
```

Note that the prelude is not automatically imported and has to be
explicitly loaded.

[Documentation]: http://www.haskell.org/haskellwiki/Ministg
