{ compiler ? "ghc865" }:

let
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs stdenv;
in
stdenv.mkDerivation {
  name = "azure-demo";

  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell.compiler.${compiler}
    pkgs.haskellPackages.hspec-discover
    pkgs.postgresql
    pkgs.xz
    pkgs.zlib
  ];

  LIBRARY_PATH = "${pkgs.xz.out}/lib:${pkgs.zlib}/lib";
}
