Rope
====

Ropes are a scalable string implementation: they are designed for
efficient operation that involve the string as a whole such as
concatenation and substring. This library implements ropes for OCaml
(it is rich enough to replace strings).

Installation
------------

The easier way to install this library is to use [opam][]:

    opam install rope

To compile the development version, you will need to install [oasis][]
and [ocamlbuild][] (comes with OCaml ≤ 4.02.3) first — say, using
`opam install oasis` — and then issue

    ocaml setup.ml -configure
    ocaml setup.ml -build

Install with:

    ocaml setup.ml -install


[opam]: http://opam.ocaml.org/
[oasis]: http://oasis.forge.ocamlcore.org/
[ocamlbuild]: http://opam.ocaml.org/packages/ocamlbuild/ocamlbuild.0/
