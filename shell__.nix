{ nixpkgs ? import <nixpkgs> {} } :

let
  inherit (nixpkgs) pkgs;
  ocamlPackages = pkgs.ocamlPackages;
  #ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_10;
  #ocamlPackages = pkgs.ocamlPackages_latest;
in

pkgs.stdenv.mkDerivation {
  name = "my-ocaml-env";
  buildInputs = [
    # ### can be provided by opam
    # ocamlPackages.dune_2
    # ##ocamlPackages.earlybird
    # #ocamlPackages.findlib
    # ocamlPackages.menhir
    # #ocamlPackages.merlin
    # ocamlPackages.ocaml
    # ##ocamlPackages.patience_diff
    # ocamlPackages.ppx_deriving
    # #ocamlPackages.ppx_expect
    # ##ocamlPackages.ppx_here
    # ocamlPackages.ppx_import
    # ##ocamlPackages.re
    # #ocamlPackages.camomile
    # #ocamlPackages.llvm
    # ocamlPackages.utop
    # #ocamlPackages.ocaml-print-intf # not available in nixpkgs
    # ocamlPackages.ocaml-lsp # not available in nixpkgs
    # #pkgs.ocamlformat

    ### tools outside of opam
    pkgs.binutils
    pkgs.gcc
    pkgs.m4

    ### needed for ocaml-lsp-server
    pkgs.clang-tools
    pkgs.llvmPackages_latest.clang

    ### needed for llvm bindings
    pkgs.llvmPackages_latest.llvm
    pkgs.python2Full
    pkgs.pkg-config
    pkgs.cmake
    pkgs.zlib
    pkgs.ncurses

    pkgs.opam

    pkgs.rlwrap

    (pkgs.emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      # pkgs.dune_2
      # pkgs.ocamlformat
    ])))

    pkgs.vscode
  ];
}
