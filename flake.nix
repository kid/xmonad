{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    xmonad-contrib.url = github:xmonad/xmonad-contrib;
  };
  outputs = { self, flake-utils, nixpkgs, xmonad-contrib }:
    let
      overlay = final: prev: rec {
        polybar-xmonad = final.symlinkJoin {
          name = "polybar-xmonad";
          paths = [ final.polybar ];
          buildInputs = [ final.makeWrapper ];
          installPhase = ''
            mkdir $out/config
            cp $src/polybar.ini $out/
          '';
          postBuild = ''
            wrapProgram $out/bin/polybar --add-flags '--config="$POLYBAR_CONFIG"'
          '';
        };
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
            (hself: hsuper:
              rec {
                xmonad-kid = (hself.callCabal2nix "xmonad-kid" (prev.nix-gitignore.gitignoreSource [ ] ./.) { }).overrideAttrs (old: rec {
                  paths = [ hself.xmonad final.polybar-xmonad ];
                  buildInputs = old.buildInputs ++ [ prev.makeWrapper ];
                  installPhase = old.installPhase + ''
                    cp $src/polybar.ini $out/
                  '';
                  postFixup = ''
                    wrapProgram "$out/bin/xmonad-kid" \
                      --prefix PATH : ${prev.lib.makeBinPath [ hself.xmonad final.polybar-xmonad ]} \
                      --set-default POLYBAR_CONFIG "$out/polybar.ini"
                  '';
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
