#! /bin/sh -x

dune build src/driver/driver.exe
dune runtest src/driver
