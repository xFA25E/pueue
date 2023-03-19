{
  inputs,
  cell,
}: let
  l = inputs.nixpkgs.lib // builtins;

  parse = import "${inputs.emacs-overlay}/parse.nix" {
    inherit (inputs.nixpkgs) lib;
    pkgs = {inherit (inputs.nixpkgs) lib;};
  };

  names =
    l.lists.filter
    (l.strings.hasSuffix ".el")
    (l.attrNames (l.readDir inputs.self));

  name = l.strings.removeSuffix ".el" (l.foldl' (acc: elm:
    if (l.stringLength elm) < (l.stringLength acc)
    then elm
    else acc) (l.head names) (l.tail names));

  mainFile = l.readFile "${inputs.self}/${name}.el";
in {
  inherit name;
  deps = parse.parsePackagesFromPackageRequires mainFile;
  url = l.elemAt (l.match ".*\n;; URL: ([^\n]+).*" mainFile) 0;
  version = l.elemAt (l.match ".*\n;; Version: ([^\n]+).*" mainFile) 0;
}
