# cabal-dependency-licenses

Compose a list of a project's dependencies with their licenses

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
