let main = 
  if Array.length Sys.argv == 3 
    then
      ()
    else 
      begin
        print_string("Type the 2 command line arguments\n");
        exit 0
      end

let soma = 
  print_string("Enter the sum values\n");
  let a = read_int() in
  let b = read_int() in
  print_string("Sum result: ");
  print_int(a + b);
  print_string("\n")

let produto = 
  let a = float_of_string Sys.argv.(1) in
  let b = float_of_string Sys.argv.(2) in
  print_string("Product result: ");
  print_float(a *. b);
  print_string("\n")


