(lang dune 2.8)

(cram enable)

(generate_opam_files true)

(source (github romildo/bcc328-2020-1))
(license MIT)
(authors "José Romildo Malaquias")
(maintainers "malaquias@gmail")

(package
 (name bcc328-2020-1)
 (synopsis "Experimenting with compiler construction")
 (description "Implementing toy programming languages as activities for an undergraduate course"))
(using menhir 2.1)
