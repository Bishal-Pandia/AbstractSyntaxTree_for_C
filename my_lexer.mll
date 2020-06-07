(* header section *)
{
  exception Lexer_exception of string
}

(* definitions section *)
let integer = ['0'-'9']['0'-'9']*
let id =  ['a'-'z' 'A'-'Z']['a'-'z' '0'-'9']*

(* rules section *)
rule scan = parse
  | [' ' '\t' '\n']+  { scan lexbuf }
  | '+'          { My_ast.Operator(Add) }
  | '-'          { My_ast.Operator(Subtract) }
  | '*'          { My_ast.Operator(Multiply) }
  | '/'          { My_ast.Operator(Divide) }
  | '='          { My_ast.Operator(Assign) }
  | integer as s { My_ast.Num_token(int_of_string s) }
  | id as s'     { My_ast.Id_token( s') }
  (* |"printf"      { My_ast.Print_Lexer } *)
  (* |"if"          { My_ast.If_Lexer } *)
  (* |"while"       { My_ast.While_Lexer) } *)
  | eof          { My_ast.EOF }

(* trailer section *)
{
}


  
