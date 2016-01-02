{ mkDerivation, aeson, base, containers, ghcjs-base, hspec, hsx2hs
, lens, mtl, stdenv, stm, text
}:
mkDerivation {
  pname = "isomaniac";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers ghcjs-base lens mtl stm text
  ];
  testHaskellDepends = [ base containers hspec hsx2hs text ];
  license = stdenv.lib.licenses.bsd3;
}
