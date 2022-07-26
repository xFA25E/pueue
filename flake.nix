{
  inputs = {
    eldev = { url = "github:doublep/eldev/1.2"; flake = false; };
  };
  outputs = { self, nixpkgs, eldev }: let
    name = "pueue";
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.eldev ]; };

    inherit (builtins) elemAt match readFile;
    mainFile = readFile "${self}/${name}.el";
    version = elemAt (match ".*\n;; Version: ([^\n]+).*" mainFile) 0;
    url = elemAt (match ".*\n;; URL: ([^\n]+).*" mainFile) 0;
  in {

    overlays = {
      default = self.overlays.${name};

      ${name} = final: prev: {
        emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope' (
          efinal: eprev: {
            ${name} = efinal.melpaBuild {
              inherit version;
              pname = name;
              src = self;
              commit = self.rev;
              recipe = final.writeText "recipe" ''
                (${name} :fetcher git :url "${url}")
              '';
              packageRequires = with efinal; [ with-editor ];
            };
          }
        );
      };

      eldev = final: prev: {
        eldev = final.stdenv.mkDerivation {
          name = "eldev";
          src = eldev;
          dontUnpack = true;
          dontPatch = true;
          dontConfigure = true;
          dontBuild = true;
          nativeBuildInputs = [ final.emacs ];
          installPhase = ''
            mkdir -p $out/bin
            cp $src/bin/eldev $out/bin/
          '';
        };
      };

    };

    devShells.${system}.default = pkgs.mkShell {
      inherit name;
      buildInputs = [ pkgs.eldev ];
      shellHook = ''
        export ELDEV_DIR=$PWD/.eldev
      '';
    };

  };
}
