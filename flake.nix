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
                llvm
                cudaPackages.cudatoolkit
                cudaPackages.cuda_nvcc
              ];
              shell.shellHook = ''
                export ACCELERATE_LLVM_CLANG=${pkgs.clang}/bin/clang
                export NIX_LDFLAGS="$NIX_LDFLAGS -L${pkgs.cudaPackages.cudatoolkit}/lib -L${pkgs.cudaPackages.cudatoolkit}/lib/stubs"

                # Runtime: real libcuda.so comes from the driver. On NixOS it's typically here.
                export LD_LIBRARY_PATH="${
                  pkgs.lib.makeLibraryPath [ pkgs.cudaPackages.cudatoolkit ]
                }:/run/opengl-driver/lib:$LD_LIBRARY_PATH"
              '';
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          config = haskellNix.config // { allowUnfree = true; };
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
