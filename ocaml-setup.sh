#! /bin/sh -x

set -e
set -u
set -o pipefail

opam_packages=(
    dune
    ppx_import
    ppx_deriving
    ppx_expect
    ppx_deriving_cmdliner
    camomile
    menhir
    ocaml-print-intf
    ocaml-lsp-server
    utop
    # llvm
)

opam init
eval $(opam env)
opam install "${opam_packages[@]}"
