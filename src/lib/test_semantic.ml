module L = Lexing

let check str =
  let lexbuf = L.from_string str in
  try
    let ast = Parser.program Lexer.token lexbuf in
    Semantic.to_funsList ast; 
  with
  | Parser.Error ->
     Format.printf "%a error: syntax\n%!" Location.pp_position lexbuf.L.lex_curr_p
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s%!" Location.pp_location loc msg

let%expect_test _ =

    check " int function(bool k) = 100
      int sum(int x) = 200 
      bool function(bool k) = true";
    [%expect{| :1.1-1.27 error: function function defined more than once |}];

    check " int function(bool k, int k) = 100";
    [%expect{| :1.31-1.34 error: typeId k defined more than once |}];

    check " int x(bool k) = k ";
    [%expect{| :1.17-1.18 error: Invalid return exp x |}];

    check " int x(int d) = 100
      bool k(bool z) = x(100) ";
    [%expect{| :2.23-2.29 error: Invalid return exp k |} ];

    check " int x(int d) = 100 
      int y(int z) = x(10) ";
    [%expect{| |}];

    check " int x(int c, int d, int e) = c + d + e + k";
    [%expect{| :1.42-1.43 error: typeId k not found in vtable |}];

    check " int x(int c, bool k) = c + k";
    [%expect{| :1.24-1.29 error: Sum exp not valid |}];

    check " bool x(int y, int z) = z < y";
    [%expect{| |}];

    check " bool x(int y, bool z) = z < y";
    [%expect{| :1.25-1.30 error: LT exp not valid |}];

    check " bool x(int y, int z) = z < y
      int k(int y, int z) = if x(y,z) then y else z";
    [%expect{| |}];

    check " bool x(int y, int z) = z < y
      int k(int y, int z, bool w) = if x(y,z) then y else w";
    [%expect{| :2.36-2.59 error: IfExp not valid |}];

    check "  int x(int d) = 100 
      int y(int z) = m(10) ";
    [%expect{| :2.21-2.26 error: Function m not found in ftable |}];
