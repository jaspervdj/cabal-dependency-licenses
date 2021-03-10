# cabal-dependency-licenses

Compose a list of a project's transitive dependencies with their licenses

**Important**: this was a quick tool I put together with a specific goal in
mind.  **It is no longer actively maintained**.  You can use [cabal-plan]
as an alternative (the `license-report` command in particular).

[cabal-plan]: https://hackage.haskell.org/package/cabal-plan#description


## Installation

Through Hackage:

    cabal install cabal-dependency-licenses

## Usage

1. Navigate to the source tree of a cabal package.

        $ cd Documents/Projects/stylish-haskell

2. Make sure the package is configured correctly.

        $ cabal configure

3. Use the tool.

        $ cabal-dependency-licenses
        # BSD3

        - aeson
        - array
        ...
        - void
        - yaml

        # MIT

        - conduit
        - text-stream-decode

        # LGPL

        - cpphs
        - polyparse
