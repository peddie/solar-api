{
  description = "Development shell for solar analysis.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/main";
    go-sungrow.url = "path:/home/peddie/programming/solar-api/go-sungrow";
  };

  outputs = { self, nixpkgs, flake-utils, go-sungrow }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        rPackages = pkgs.rWrapper.override {
          packages = with pkgs.rPackages; [
            # Tidyverse
            tidyverse
            modelr
            data_table
            reshape2
            slider
            furrr
            RcppRoll

            # Stan stuff
            rstan
            brms
            pkgs.cmdstan

            # Bayesian stuff
            tidybayes
            bayesplot
            dagitty
            coda
            posterior

            # Visualization
            ggplot2
            raster
            cowplot
            gridExtra
            ggrepel
            RColorBrewer
            leaflet
            hexbin

            # Shiny
            shiny
            shinystan
            shinythemes
            shinytoastr
            shinypanels
            shinybusy
            plotly

            # Linear algebra
            RSpectra
            EigenR
            RcppEigen
            Rcpp
            Rcpp11

            # Stats
            dqrng
            invgamma

            # Spatial stuff
            sp
            sf
            rgdal

            # Disk I/O
            qs

            # Output
            knitr
            roxygen2
            docstring
            progressr

            # Development
            lintr
            reprex
            roger
            styler
            tictoc
            lobstr
            microbenchmark
            devtools
            # TODO(MP): This package ends up with C++ linker errors.
            (buildRPackage {
              name = "cmdstanr";
              src = pkgs.fetchurl {
                url = "https://mc-stan.org/r-packages/src/contrib/cmdstanr_0.5.3.tar.gz";
                sha256 = "DF3BauB8DT7M9AEth3BXZlF0zuxosnKYxF+HPhRmbCM=";
              };
              propagatedBuildInputs = [ checkmate data_table jsonlite posterior processx R6 ];
            })
          ];
        };
        packageName = "solar-api";
      in {
        packages.default = pkgs.R;

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            git
            glibcLocales
            openssl
            which
            openssh
            curl
            wget
            file
            rPackages
            go-sungrow.packages.${system}.go-sungrow
          ];
          shellHook = ''
    mkdir -p "$(pwd)/_libs"
    export R_LIBS_USER="$(pwd)/_libs"
    echo ${rPackages}/bin/R
  '';
        };
      });

}
