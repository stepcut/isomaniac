{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, containers, ghcjs-base, mtl
      , stdenv, stm, text, hspec, hsx2hs, lens, random, Cabal
      }:
      mkDerivation {
        pname = "isomaniac";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base Cabal containers ghcjs-base mtl stm text hsx2hs lens random hspec
        ];
        testHaskellDepends = [ base containers hspec hsx2hs text ];
        buildTools = [ pkgs.haskellPackages.cabal-install ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
