(* Driver *)

(* create lexing buffer *)
let new_lexbuf name_opt =
  match name_opt with
  | None ->
     Lexing.from_channel stdin
  | Some name ->
     let lexbuf = Lexing.from_channel (open_in name) in
     Lexing.set_filename lexbuf name;
     lexbuf

(* function to show remaining tokens from lexing buffer *)
let rec show_remaining_tokens lexbuf =
  let tok = Lexer.token lexbuf in
  Format.printf
    "%a %s\n"
    Location.pp_location (Location.curr_loc lexbuf)
    (Lexer.show_token tok);
  match tok with
  | Parser.EOF -> ()
  | _ -> show_remaining_tokens lexbuf

let main () =
  Cmdline.parse ();

  let lexbuf = new_lexbuf (Cmdline.get_filename ()) in

  try
    if Cmdline.get_lexer () then
      show_remaining_tokens lexbuf
  with
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s\n" Location.pp_location loc msg;
     exit 1

let _ =
  main ()
