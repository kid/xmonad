{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    xmonad-contrib.url = github:xmonad/xmonad-contrib;
  };
  outputs = { self, flake-utils, nixpkgs, xmonad-contrib }:
    let
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
            (hself: hsuper: {
              xmonad-kid =
                hself.callCabal2nix "xmonad-kid"
                  (nixpkgs.lib.sourceByRegex ./.
                    [
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
          defaultPackage = pkgs.haskellPackages.xmonad-kid;
          # defaultPackage = pkgs.haskellPackages.xmonad-kid.overrideAttrs (old: rec {
          #   nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.makeWrapper ];
          #   installPhase = old.installPhase + ''
          #     ln -s ${pkgs.haskellPackages.xmonad-kid}/bin/xmonad-kid $out/bin/xmonad-${system}
          #   '';
          #   # postFixup = ''
          #   #   wrapProgram $out/bin/xmonad-${system} --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.haskellPackages.xmobar]}
          #   # '';
          # });
        }) // { inherit overlay overlays; };
}
