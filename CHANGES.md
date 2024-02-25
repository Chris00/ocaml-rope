0.6.3 2024-02-25
----------------

- Compatibility with OCaml 5.

0.6.2 2019-03-19
----------------

- Improve the structure of the documentation.
- Use [dune](https://github.com/ocaml/dune) to compile.
- Upgrade to OPAM 2.

0.6.1 2017-12-25
----------------

- Sync the license headers with `LICENSE.md`.

0.6 2017-11-17 
--------------

- Enrich the library to have all functions of the `String` module.
- Add `rope.top` findlib library so one can just do `#require
  "rope.top";;` in the toplevel (REPL).
- Compatible with `-safe-string` and code improvements based on string
  immutability.
- Port to `jbuilder`.
- Fix all warnings.
- Clarify the license for files in `bench/` and fix FSF address.
