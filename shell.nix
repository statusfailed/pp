{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, free, random-fu, stdenv }:
      mkDerivation {
        pname = "pp";
        version = "0.1.0.0";
        src = /home/sf/projects/academic/pp;
        libraryHaskellDepends = [ base free random-fu ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
