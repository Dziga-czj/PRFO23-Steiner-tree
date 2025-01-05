#!/bin/sh
ocamlc -I /usr/lib/x86_64-linux-gnu/ocaml/5.2.0/graphics  -dllpath /usr/lib/x86_64-linux-gnu/ocaml/5.2.0/stublibs/ -I /usr/lib/x86_64-linux-gnu/ocaml/5.2.0/stublibs/ graphics.cma "$@"
