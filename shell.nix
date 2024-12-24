{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    cabal-install
    ghc
    haskell.packages.ghc8107.aeson
    haskell.packages.ghc8107.bytestring
    haskell.packages.ghc8107.constraints
    haskell.packages.ghc8107.constraints-extras
    haskell.packages.ghc8107.envparse
    haskell.packages.ghc8107.raw-strings-qq
    haskell.packages.ghc8107.safe
    haskell.packages.ghc8107.some
    haskell.packages.ghc8107.text
    haskell.packages.ghc8107.uuid
    haskell.packages.ghc8107.optparse-applicative
  ];
}
