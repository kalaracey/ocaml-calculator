(lang dune 3.12)

(name interp)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name interp)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ounit2)
 (tags
  (topics "to describe" your project)))

; https://dune.readthedocs.io/en/stable/reference/dune/menhir.html
(using menhir 2.1)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
