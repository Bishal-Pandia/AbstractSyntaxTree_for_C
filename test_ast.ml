(*y=x+2;*)
let test_10 () = 
  let e = [
    my_ast.Expression_generic(my_ast.BinaryOp(my_ast.Id("y"), my_ast.Assign, my_ast.BinaryOp(my_ast.Id("x"), my_ast.Add, my_ast.Num(2))))
  ] in
  my_ast.parser_main e;;