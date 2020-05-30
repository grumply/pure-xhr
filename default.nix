{ mkDerivation, base, lens, wreq, pure-lifted, pure-json, pure-txt, pure-uri, stdenv 
}:
mkDerivation {
  pname = "pure-xhr";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens wreq pure-lifted pure-json pure-txt pure-uri ];
  homepage = "github.com/grumply/pure-xhr";
  license = stdenv.lib.licenses.bsd3;
}