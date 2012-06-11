# hsporter :: PackageDescription -> FreeBSDPort

## Installation

    runhaskell Setup.lhs configure
    runhaskell Setup.lhs build
    runhaskell Setup.lhs haddock --executable (for documentation)
    runhaskell Setup.lhs install

## Usage

### Synopsis

    hsporter cabal_description_url [category]

    where
      cabal_description_url = valid URL of a Cabal package (probably on HackageDB)
      category = FreeBSD port category

### Example

Issue the following command.

      hsporter http://hackage.haskell.org/packages/archive/DeepArrow/0.3.3/DeepArrow.cabal

It will create all the files needed for a port inside the directory `devel/hs-DeepArrow`.


# Disclaimer

Please, do not forget to test and use the ports before doing anything with it in public.
Note that it is very experimental and under development.  Slippery when wet.

# Contact

FreeBSD Haskell People <haskell@FreeBSD.org>
