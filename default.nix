{ mkDerivation, aeson, base, containers, ghcjs-base, mtl, stdenv
, stm, text
}:
mkDerivation {
  pname = "isomaniac";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers ghcjs-base mtl stm text
  ];
  license = stdenv.lib.licenses.bsd3;
}
