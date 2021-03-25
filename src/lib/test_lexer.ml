(* expect tests for the scanner *)

module L = Lexing

let scan_string s =
  let lexbuf = L.from_string s in
  let rec go () =
    let tok = Lexer.token lexbuf in
    Format.printf
      "%a %s\n%!"
      Location.pp_location (Location.curr_loc lexbuf)
      (Lexer.show_token tok);
    match tok with
    | Parser.EOF -> ()
    | _ -> go ()
  in
  try go ()
  with
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s\n" Location.pp_location loc msg

let%expect_test _ =
  (* spaces *)
  scan_string " \n\t   \n  ";
  [%expect{| :3.2-3.2 Parser.EOF |}];

  (* integer literal *)
  scan_string "27348";
  [%expect{|
    :1.0-1.5 (Parser.LITINT 27348)
    :1.5-1.5 Parser.EOF |}];

  (* integer literal has no signal *)
  scan_string "-27348";
  [%expect{| :1.0-1.1 error: illegal character '-' |}];

  scan_string "+27348";
  [%expect{|
    :1.0-1.1 Parser.PLUS
    :1.1-1.6 (Parser.LITINT 27348)
    :1.6-1.6 Parser.EOF |}];

  (* types *)
  scan_string "int bool";
  [%expect{|
    :1.0-1.3 Parser.INT
    :1.4-1.8 Parser.BOOL
    :1.8-1.8 Parser.EOF |}];

  (* let *)
  scan_string "let in";
  [%expect{|
    :1.0-1.3 Parser.LET
    :1.4-1.6 Parser.IN
    :1.6-1.6 Parser.EOF |}];

  (* if *)
  scan_string "if then else";
  [%expect{|
    :1.0-1.2 Parser.IF
    :1.3-1.7 Parser.THEN
    :1.8-1.12 Parser.ELSE
    :1.12-1.12 Parser.EOF |}];

  (* identifier *)
  scan_string "Idade alfa15 beta_2";
  [%expect{|
    :1.0-1.5 (Parser.ID "Idade")
    :1.6-1.12 (Parser.ID "alfa15")
    :1.13-1.19 (Parser.ID "beta_2")
    :1.19-1.19 Parser.EOF |}];

  (* invalid identifier *)
  scan_string "_altura";
  [%expect{| :1.0-1.1 error: illegal character '_' |}];

  scan_string "5x";
  [%expect{|
    :1.0-1.1 (Parser.LITINT 5)
    :1.1-1.2 (Parser.ID "x")
    :1.2-1.2 Parser.EOF |}];

  (* operators *)
  scan_string "+ <";
  [%expect{|
    :1.0-1.1 Parser.PLUS
    :1.2-1.3 Parser.LT
    :1.3-1.3 Parser.EOF |}];

  (* punctuation *)
  scan_string "( ) , =";
  [%expect{|
    :1.0-1.1 Parser.LPAREN
    :1.2-1.3 Parser.RPAREN
    :1.4-1.5 Parser.COMMA
    :1.6-1.7 Parser.EQ
    :1.7-1.7 Parser.EOF |}]
