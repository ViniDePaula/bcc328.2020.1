{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char
}

let spaces = [' ' '\t']+
let digit = ['0'-'9']
let integer = digit+
let identifier = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] *
(* add other definitions, if needed *)

rule token = parse
  | spaces            { token lexbuf }
  | '\n'              { L.new_line lexbuf; token lexbuf }
  | integer as lxm    { LITINT (int_of_string lxm) }
  (* add other lexical rules *)

  (* types *)
  | "int"             { INT }
  | "bool"            { BOOL }

  (* let *)
  | "let"             { LET }
  | "in"              { IN }

  (* if *)
  | "if"              { IF }
  | "then"            { THEN }
  | "else"            { ELSE }

  (* identifier *)
  | identifier as lxm { ID ( Symbol.name(Symbol.symbol(lxm)))}

  (* operators *)
  | "<"               { LT }
  | '+'               { PLUS }

  (* punctuation *)
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | ','               { COMMA }
  | '='               { EQ }


  | eof               { EOF }
  | _                 { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }
