{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
in {
  eldev = nixpkgs.stdenv.mkDerivation {
    name = "eldev";
    src = inputs.eldev;
    dontUnpack = true;
    dontPatch = true;
    dontConfigure = true;
    dontBuild = true;
    nativeBuildInputs = [nixpkgs.emacs];
    installPhase = ''
      mkdir -p $out/bin
      cp $src/bin/eldev $out/bin/
    '';
  };
}
