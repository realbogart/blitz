{
  description = "";

  inputs = {
    nixpkgs.url =
      "github:nixos/nixpkgs/041c867bad68dfe34b78b2813028a2e2ea70a23c";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (self: super: {
            c = self.glibc;
            dl = self.glibc;
          })
          (final: prev: {
            blitz = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc9101";
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
              };
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
                ghcid
                ormolu
                pkg-config
              ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.blitz.flake { };
      in flake // {
        packages = flake.packages // {
          default = flake.packages."blitz:exe:blitz";
          container = pkgs.dockerTools.buildLayeredImage {
            name = "blitz";
            # tag = self.rev or "dirty";
            tag = "latest"; # TODO: Better tagging
            contents = [ flake.packages."blitz:exe:blitz" ./web ];
            config = {
              Cmd = [ ];
              Entrypoint = [ "/bin/blitz" ];
            };
          };
        };
      });
}
