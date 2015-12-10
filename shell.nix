{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, containers, ghcjs-base, mtl
      , stdenv, stm, text, hsx2hs, lens, random
      }:
      mkDerivation {
        pname = "isomaniac";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base containers ghcjs-base mtl stm text hsx2hs lens random
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
