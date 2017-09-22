{ mkDerivation, aeson, barrier, base, bytestring, containers, flow
, JuicyPixels, lens, lens-aeson, Rasterific, rasterific-svg, scotty
, stdenv, svg-tree, text, wreq, xml
}:
mkDerivation {
  pname = "hydra-badges";
  version = "0.1.0";
  src = ../..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson barrier base bytestring containers flow JuicyPixels lens
    lens-aeson Rasterific rasterific-svg scotty svg-tree text wreq xml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/taktoa/hydra-badges";
  description = "A server for Hydra jobset status badges";
  license = stdenv.lib.licenses.mit;
}
