{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    xmonad-contrib.url = github:xmonad/xmonad-contrib;
    xmonad-dbus.url = github:troydm/xmonad-dbus;
    xmonad-dbus.flake = false;
  };
  outputs = inputs @ { self, flake-utils, nixpkgs, xmonad-contrib, ... }:
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
                    { }).overrideAttrs (old: rec { });
                # Disable tests on xmonad-dbus, as it fails to load the DBUS_SESSION_BUS_ADDRESS correctly
                xmonad-dbus = hself.callCabal2nixWithOptions "xmonad-dbus"
                  (prev.nix-gitignore.gitignoreSource [ ] inputs.xmonad-dbus.outPath) "--no-check"
                  { };
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
              pkgs.haskellPackages.cabal-fmt
              pkgs.haskellPackages.fourmolu
            ];
          };

          packages = flake-utils.lib.flattenTree {
            xmonad = pkgs.haskellPackages.xmonad;
            xmonad-dbus = pkgs.haskellPackages.xmonad-dbus;
            xmonad-kid = pkgs.haskellPackages.xmonad-kid;
            polybar-xmonad = pkgs.polybar-xmonad;
          };

          defaultPackage = packages.xmonad-kid;
        }) // { inherit overlay overlays; };
}
