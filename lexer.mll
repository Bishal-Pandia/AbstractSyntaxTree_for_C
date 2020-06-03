{
exception Lexer_exception of string
}

let integer = ['0'-'9']['0'-'9']*

rule scan = parse
  | [' ' '\t' '\n']+  { scan lexbuf }
  | '+'          { My_ast.Operator(Add) }
  | '-'          { My_ast.Operator(Subtract) }
  | '*'          { My_ast.Operator(Multiply) }
  | '/'          { My_ast.Operator(Divide) }
  | '='          { My_ast.Operator(Assign) }
  | integer as s { My_ast.Num_token(int_of_string s) }
  | eof          { My_ast.EOF }

{
}


(* |"printf"        { My_ast.Print_Lexer }
  |"if"          { My_ast.If_Lexer }
  |"while"       { My_ast.While_Lexer } *)
