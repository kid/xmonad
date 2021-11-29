{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    xmonad-contrib.url = github:xmonad/xmonad-contrib;
  };
  outputs = { self, flake-utils, nixpkgs, xmonad-contrib }:
    let
      overlay = final: prev: rec {
        polybar-xmonad = final.stdenv.mkDerivation {
          name = "polybar-xmonad";
          src = self;
          buildInputs = [ final.makeWrapper final.polybar ];
          installPhase = ''
            mkdir -p $out/bin $out/share
            ln -s $src/polybar.ini $out/share/polybar.ini
            makeWrapper ${final.polybar}/bin/polybar $out/bin/polybar-xmonad --add-flags --config="$out/share/polybar.ini"
          '';
        };
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
            (hself: hsuper:
              rec {
                xmonad-kid = (
                  hself.callCabal2nix "xmonad-kid"
                    (prev.nix-gitignore.gitignoreSource [ ] ./.)
                    { }).overrideAttrs (old: rec {
                });
              });
        });
      };
      overlays = xmonad-contrib.overlays ++ [ overlay ];
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system overlays; };
        in
        rec {
          devShell = pkgs.haskellPackages.shellFor {
            packages = p: [ p.xmonad-kid ];
            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.haskell-language-server
            ];
          };

          packages = flake-utils.lib.flattenTree {
            xmonad = pkgs.haskellPackages.xmonad;
            xmonad-kid = pkgs.haskellPackages.xmonad-kid;
            polybar-xmonad = pkgs.polybar-xmonad;
          };

          defaultPackage = packages.xmonad-kid;
        }) // { inherit overlay overlays; };
}
