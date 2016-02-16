{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, free, machines, mtl, random
      , random-fu, stdenv, transformers
      }:
      mkDerivation {
        pname = "pp";
        version = "0.1.0.0";
        src = /home/sf/projects/academic/pp;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers free machines mtl random random-fu transformers
        ];
        executableHaskellDepends = [
          base containers free machines mtl random random-fu transformers
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
