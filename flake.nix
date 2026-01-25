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

              shell.buildInputs = with final; [
                nixpkgs-fmt
                ghcid
                ormolu
                pkg-config
                llvm
                llvmPackages.clang-unwrapped
                cudaPackages.cudatoolkit
                cudaPackages.cuda_nvcc
              ];

              shell.shellHook = ''
                export ACCELERATE_LLVM_CLANG=${final.llvmPackages.clang-unwrapped}/bin/clang
                export NIX_LDFLAGS="$NIX_LDFLAGS -L${final.cudaPackages.cudatoolkit}/lib -L${final.cudaPackages.cudatoolkit}/lib/stubs"
                export LD_LIBRARY_PATH="${
                  final.lib.makeLibraryPath [ final.cudaPackages.cudatoolkit ]
                }:/run/opengl-driver/lib:$LD_LIBRARY_PATH"
              '';

              modules = [
                ({ pkgs, ... }: {
                  packages.accelerate-llvm.prePatch = ''
                    if [ ! -f LICENSE ]; then
                      echo "Missing upstream LICENSE file; placeholder added by Nix build." > LICENSE
                    fi
                  '';

                  packages.accelerate-llvm-native.prePatch = ''
                    if [ ! -f LICENSE ]; then
                      echo "Missing upstream LICENSE file; placeholder added by Nix build." > LICENSE
                    fi
                  '';

                  packages.accelerate-llvm-ptx.prePatch = ''
                    if [ ! -f LICENSE ]; then
                      echo "Missing upstream LICENSE file; placeholder added by Nix build." > LICENSE
                    fi
                  '';
                })
                ({ pkgs, ... }: {
                  packages.cuda.components.library = {
                    build-tools = [ pkgs.cudaPackages.cuda_nvcc ];

                    preConfigure = ''
                      export CUDA_PATH=${pkgs.cudaPackages.cudatoolkit}
                      export PATH=${pkgs.cudaPackages.cuda_nvcc}/bin:$PATH
                    '';

                    configureFlags = [
                      "--extra-include-dirs=${pkgs.cudaPackages.cudatoolkit}/include"
                      "--extra-lib-dirs=${pkgs.cudaPackages.cudatoolkit}/lib"
                      "--extra-lib-dirs=${pkgs.cudaPackages.cudatoolkit}/lib64"
                      "--extra-lib-dirs=${pkgs.cudaPackages.cudatoolkit}/lib/stubs"
                    ];
                  };
                })
              ];
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
