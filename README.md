https://cs3110.github.io/textbook/chapters/interp/parsing.html

```bash
dune build
opam install --deps-only .
```

Then run `dune utop lib`:

```ocaml
utop # Interp.Main.interp_small "let x = 3110 in x + x";;
- : Interp.Ast.expr = Interp.Ast.Int 6220
```
