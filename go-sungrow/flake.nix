{
  description = "Go interface to Sungrow solar inverters";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    go-sungrow-repo = {
      url = "github:MickMake/GoSungrow";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        go-sungrow = pkgs.buildGoModule {
          pname = "GoSungrow";
          version = "3.0.4";
          src = inputs.go-sungrow-repo;
          vendorHash = "sha256-fIlxHJfz7Sm7SHZFVkwgfuhiB1gBLcMW0+IpQ0cgbiM=";
          proxyVendor = true;

          doCheck = false;

          meta = with pkgs.lib; {
            description =
              "GoLang implementation to access the iSolarCloud API updated by SunGrow inverters";
            homepage = "https://github.com/MickMake/GoSungrow";
            license = licenses.gpl2;
          };
        };

        app = {
          type = "app";
          program = "${go-sungrow}/bin/GoSungrow";
        };
      in rec {
        packages.go-sungrow = go-sungrow;
        packages.default = go-sungrow;
        apps.go-sungrow = app;
        apps.default = app;
        devShells.go-sungrow = pkgs.mkShell { buildInputs = [ packages.${system}.go-sungrow ]; };
        devShells.default = devShells.${system}.go-sungrow;
      });
}
