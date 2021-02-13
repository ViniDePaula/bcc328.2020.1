(* handling command line arguments: interface *)

val get_filename : unit -> string option
val get_lexer : unit -> bool

val parse : unit -> unit
