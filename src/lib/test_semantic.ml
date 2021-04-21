module L = Lexing

let check str =
  let lexbuf = L.from_string str in
  try
    let ast = Parser.program Lexer.token lexbuf in
    Semantic.check_program ast; 
  with
  | Parser.Error ->
     Format.printf "%a error: syntax\n%!" Location.pp_position lexbuf.L.lex_curr_p
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s%!" Location.pp_location loc msg

let%expect_test _ =

    check " int function(int x, int y) = x + y
      bool z(int k) = k < function(10, 20) 
      int main(int x) = 
        if z(x) 
          then 10 + function(x, 100)
        else
          10";
    [%expect{| |}];            (*testing correct main call*)

    check " int function(int x) = 10
      bool z(int k) = k < function(k)";
    [%expect{| :1.1-2.37 error: Main function not found |}];  (*testing undeclared main function*)

    check " int function(int x) = 10
      bool main(int x) = x < function(10)";
    [%expect{| :1.1-2.41 error: Main function must be type Int |}];   (*testing incorrect type of main function*)
  
    check " int function(bool k) = 100
      int sum(int x) = 200 
      bool function(bool k) =  sum(10) < function(true)";
    [%expect{| :1.1-1.27 error: function function defined more than once |}];   (*testing function id defined more than once*)

    check " int function(bool k, int k) = 100";
    [%expect{| :1.31-1.34 error: typeId k defined more than once |}];   (*testing type id defined more than once*)

    check " int x(bool k) = k ";
    [%expect{| :1.17-1.18 error: Invalid return exp x |}];              (*testing incorrect type return in exp*)

    check " int x(int d) = 100
      bool k(bool z) = x(100) ";
    [%expect{| :2.23-2.29 error: Invalid return exp k |} ];             (*testing incorrect type return in exp*)

    check " int x(int c, int d, int e) = c + d + e + k";
    [%expect{| :1.42-1.43 error: typeId k not found in vtable |}];      (*testing non-existent variable in vtable*)

    check " int x(int c, bool k) = c + k";
    [%expect{| :1.24-1.29 error: Sum exp not valid |}];                 (*testing sum with diferent types*)

    check " bool x(int y, int z) = z < y
      int main(int x) = 10";
    [%expect{| |}];                                                     (*testing correct lt exp*)

    check " bool x(int y, bool z) = z < y
      int main(int x) = 10";
    [%expect{| :1.25-1.30 error: LT exp not valid |}];                  (*testing less than with diferent types*)

    check " bool x(int y, int z) = z < y
      int k(int y, int z) = if x(y,z) then y else z
      int main(int x) = 10";
    [%expect{| |}];                                                     (*testing correct if exp*)

    check " bool x(int y, int z) = z < y
      int k(int y, int z, bool w) = if x(y,z) then y else w";
    [%expect{| :2.36-2.59 error: IfExp not valid |}];                   (*testing different types of return in the if exp*)

    check "  int x(int d) = 100 
      int y(int z) = m(10) ";
    [%expect{| :2.21-2.26 error: Function m not found in ftable |}];    (*testing undeclared function call*)

    check " int function(int a, int b, bool m) = a + b
      int main(int x) = function(x, 10, 10 < 11)";
    [%expect{| |}];                                                     (*testing correct call function*)

    check " int function(int a, int b, bool m) = a + b
      int main(int x) = function(x, 10 < 11)";
    [%expect{| :2.24-2.44 error: Invalid params size |}];               (*testing params size at function call*)

    check " int function(int a) = a + 10
      int main(bool x) = function(x)";
    [%expect{| :2.25-2.36 error: Param type not match |}];              (*testing function call params *)

