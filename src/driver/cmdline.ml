(* handling command line arguments: implementation *)

let filename = ref None
let lexer = ref true

let get_filename () =
  !filename

let set_filename name =
  filename := Some name

let get_lexer () =
  !lexer

let usage_msg =
  "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... FILE\n"

let rec usage () =
  Arg.usage options usage_msg;
  exit 0

and options =
  [ "-h",      Arg.Unit usage, "\tDisplay an usage message"
  ; "--help",  Arg.Unit usage, "\tDisplay an usage message"
  ; "--lexer", Arg.Set lexer,  "\tDisplay sequence of lexical symbols"
  ]

let parse () =
  Arg.parse options set_filename usage_msg
