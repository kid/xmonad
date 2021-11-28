{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    xmonad-contrib.url = github:xmonad/xmonad-contrib;
  };
  outputs = { self, flake-utils, nixpkgs, xmonad-contrib }:
    let
      lib = nixpkgs.lib;
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
            (hself: hsuper: {
              xmonad-kid =
                hself.callCabal2nix "xmonad-kid"
                  # TODO use nixpkgs's gitignore function
                  (nixpkgs.lib.sourceByRegex ./.
                    [
                      "polybar.ini"
                      "xmonad.hs"
                      "xmonad-kid.cabal"
                    ])
                  { };
            });
        });
      };
      overlays = xmonad-contrib.overlays ++ [ overlay ];
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = import nixpkgs { inherit system overlays; };
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
            xmonad-kid = pkgs.haskellPackages.xmonad-kid.overrideAttrs (old: rec {
              nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.makeWrapper pkgs.polybar ];
              installPhase = old.installPhase + ''
                mkdir -p $out/.config/polybar
                cp $src/polybar.ini $out/.config/
              '';
              # TODO use `--set-default` instead?
              postFixup = ''
                wrapProgram $out/bin/xmonad-kid \
                  --prefix PATH : ${lib.makeBinPath [pkgs.xmobar pkgs.polybar]} \
                  --set XDG_CONFIG_HOME "$out/.config"
              '';
            });
          };

          defaultPackage = packages.xmonad-kid;
        }) // { inherit overlay overlays; };
}
