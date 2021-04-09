with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "pueue-dev-env";
  buildInputs = [ eldev ];
}
