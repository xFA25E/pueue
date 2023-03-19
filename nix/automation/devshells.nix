{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
in {
  default = nixpkgs.mkShell {
    packages = [cell.packages.eldev nixpkgs.alejandra nixpkgs.statix];
    shellHook = ''
      export ELDEV_DIR=$PWD/.eldev
    '';
  };
}
