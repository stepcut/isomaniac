{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, ghcjs-base, hsx2hs, mtl, stdenv, stm
      , text
      }:
      mkDerivation {
        pname = "isomaniac";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ base ghcjs-base hsx2hs mtl stm text ];
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
