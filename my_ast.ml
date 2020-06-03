(*For Exception Handling*)
exception ParseError of string

(* open Printf *)

(*Type Definitions*)
type operator = 
  Add
| Subtract
| Multiply
| Divide
| Assign  

(* let rec fprintfoperator out l = 
  match l with
    Add -> fprintf out "+"
  | Subtract -> fprintf out "-"
  | Multiply -> fprintf out "*"
  | Divide -> fprintf out "/"
  | Assign -> fprintf out "=" *)

type conditional_operator = 
  Equal
| Lessthan
| Greaterthan

(* let rec fprintfcond_op out l =
  match l with
    Equal -> fprintf out "=="
  | Lessthan -> fprintf out "<"
  | Greaterthan -> fprintf out ">" *)

type token =
  Operator of operator
| Num_token of int
| Id_token of string
| EOF 

(* let rec fprintftoken out l = 
  match l with
    Operator(op) -> fprintf out "%a" fprintfoperator op
  | Num_token(i) -> fprintf out "%d" i
  | Id_token(s) -> fprintf out "%s" s *)

type expression = 
  Num of int
| Id of string
| BinaryOp of expression * operator * expression

(* let rec fprintfexpr out l =
  match l with
    Num(i) -> fprintf out "%d" i 
  | Id(s) -> fprintf out "%s" s
  | BinaryOp(l1, op, l2) -> fprintf out "%a %a %a" fprintfexpr l1, fprintfoperator op, fprintfexpr l2 *)

type condition = 
  expression * conditional_operator * expression

(* let rec fprintfcond out l =
  match l with
    (ex, cond_op, ex2) -> fprintf out "%a %a %a" fprintfexpr ex, fprintfcond_op cond_op, fprintfexpr ex2  *)

type my_print = 
  Print_Lexer

(* let rec fprintfprint out l = 
  match l with
    Print_Lexer -> fprintf out "printf" *)

type my_while = 
  While_Lexer

(* let rec fprintfwhile out l = 
  match l with
    While_Lexer -> fprintf out "while" *)

type my_if = 
  If_Lexer

(* let rec fprintfif out l = 
  match l with
    If_Lexer -> fprintf out "if" *)
  
type others = 
  Semicolon
| Comma
| Lparen
| Rparen

type generic_object = 
  Print_generic of my_print
| While_generic of my_while
| If_generic of my_if
| Token_generic of token list
| Expression_generic of expression
| Condition_generic of condition
| Others of others

(* let rec fprintfgeneric out h =
  match h with
    Print_generic(h) -> fprintf out "%a" fprintfprint h
  | While_generic(h) -> fprintf out "%a" fprintfwhile h
  | If_generic(h) -> fprintf out "%a" fprintfif h
  | Token_generic(h) -> fprintf out "%a" fprintftoken h  *)

(* let rec fprintfgeneric out l =
  match l with
    [] -> fprintf out "Empty!"
  | h::t ->
    begin
      match h with
        Print_generic(h) -> fprintf out "%a" fprintfprint h
      | While_generic(h) -> fprintf out "%a" fprintfwhile h
      | If_generic(h) -> fprintf out "%a" fprintfif h
      | Token_generic(h) -> fprintf out "%a" fprintftoken t 
    end *)
      (* | Expression_generic(l) -> fprintf out "%a" fprintfexpr l *)
      (* | Condition_generic(l) -> fprintf out "%a" fprintfcond l *)

type return_values = 
  Generic_print of my_print * generic_object
| Generic_while of condition * generic_object
| Generic_if of condition * generic_object * generic_object
| Generic_only_if of condition * generic_object
| Generic_expression of expression

(* let rec fprintreturn out l=
  match l with 
    Generic_print(p, g) -> fprintf out "%a %a" fprintfprint p, fprintfgeneric g
  | _ -> fprintf out "Not now"
  (* | Generic_while(c, g) -> fprintf out "%a %a" fprintfcond c, fprintfgeneric g *)
  (* | Generic_if(c, g, g2) -> fprintf out "%a %a %a" fprintfcond c, fprintfgeneric g, fprintfgeneric g2 *)
  (* | Generic_only_if(c, g) -> fprintf out "%a %a" fprintfcond c, fprintfgeneric g *)
  (* | Generic_expression(ex) -> fprintf out "%a" fprintfexpr ex *)

[@@deriving show] *)


(*************************************************************************************************)

(*PARSER FUNCTIONS BEGIN!*)


(*Mathematical Expressions in Prefix Form*)
let rec parser_expression l = 
  match l with
    [] -> raise(ParseError "Error! Empty String Found")
  | h::t -> 
      match h with 
        Operator(token) ->
          let (h', t') = parser_expression t in 
            let (h'', t'') = parser_expression t' in
              (BinaryOp(h', token, h''), t'')
      | Num_token(token) -> (Num(token), t)
      | Id_token(token) -> (Id(token), t)
      | EOF -> raise(ParseError "Error! Reached end of string") 


(*Mathematical Expressions in Infix Form*)
(*Here we simply convert the expression from Infix to Prefix*)
let rec infix_to_prefix l =
  match l with  
    [] -> raise(ParseError "Error! Empty String Found")
  | h::t ->
      match t with 
        [] -> h::[]
      | h'::t' -> 
          match h' with 
            Operator(token) -> h'::h::infix_to_prefix t'
          | Num_token(token) -> raise(ParseError "Error! Invalid String type")
          | Id_token(token) -> raise(ParseError "Error! Invalid String type")
          | EOF -> raise(ParseError "Error! Reached end of string") 


(* *)          
let wrapper l = 
  let prefix_list = infix_to_prefix l in
    let (binaryop, list) = parser_expression prefix_list in
      Expression_generic(binaryop) 


(* This is the parser function for print statements*)      
let parser_print h t =
  match h with 
    Print_generic(token) ->
      begin
        match t with 
          Token_generic(token') -> 
            let binaryop = wrapper token' in
              Generic_print(token, binaryop)
              (* print_endline s *)
              (* print_endline (show t) *)
        | Expression_generic(_) ->
            Generic_print(token, t)
        | _ -> raise(ParseError "Error! Invalid type")  
      end 
  | _ -> raise(ParseError "Error! Invalid type") 
 

(* This is the parser function for while loop *) 
let parser_while cond exe = 
  match cond with
    Condition_generic(token) ->
      begin
        match exe with 
          Token_generic(token') ->
            let binaryop = wrapper token' in
              Generic_while(token, binaryop)
        | Expression_generic(token') ->
            Generic_while(token, exe)
        | _ -> raise(ParseError "Error! Invalid Type!")
      end
  | _ ->  raise(ParseError "Error! Invalid Type")


(* This is the parser function for only if conditional statement which is not followed by an else condition *)
let parser_only_if cond true_case_exe =
  match cond with
    Condition_generic(token) -> 
      begin
        match true_case_exe with
          Token_generic(token') ->
            let binaryop_true_case = wrapper token' in
              Generic_only_if(token, binaryop_true_case)
        | Expression_generic(token') ->
            Generic_only_if(token, true_case_exe)
        | _ -> raise(ParseError "Error! Invalid Type")
      end
  | _ -> raise(ParseError "Error! Invalid Type")

(* This is the parser function for if-else conditional statements where a if condition is followed by a else condition *)
let parser_if cond true_case_exe false_case_exe = 
  match cond with
    Condition_generic(token) -> 
      begin
        match true_case_exe with
          Token_generic(token') ->
            begin
              match false_case_exe with
                Token_generic(token'') ->
                  let binaryop_true_case = wrapper token' in
                    let binaryop_false_case = wrapper token'' in
                      Generic_if(token, binaryop_true_case, binaryop_false_case)
              | Expression_generic(token'') ->
                  let binaryop_true_case = wrapper token' in
                    Generic_if(token, binaryop_true_case, false_case_exe)
              | _ -> raise(ParseError "Error! Invalid Type!")
            end  
        | Expression_generic(token') -> 
            begin
              match false_case_exe with
                Token_generic(token'') -> 
                  let binaryop_false_case = wrapper token'' in
                    Generic_if(token, true_case_exe, binaryop_false_case)
              | Expression_generic(token'') -> 
                  Generic_if(token, true_case_exe, false_case_exe)
              | _ -> raise(ParseError "Error! Invalid Type")
            end
        | _ -> raise(ParseError "Error! Invalid Type")
      end  
  | _ -> raise(ParseError "Error! Invalid Type")
  
(* let printreturn = fprintreturn stdout
let result = [] *)

(*The main pareser function incorporating all the above parser functions *)
let rec parser_main lex_list = 
  match lex_list with
    [] -> []
  | h::t -> 
      begin
        match h with
          (* Check with print*) 
          Print_generic(_) -> 
            begin
              match t with 
                [] -> raise(ParseError "Error! Empty")
              | h'::t' -> 
                  begin
                    match h' with
                      Token_generic(_)|Expression_generic(_)  ->
                        (parser_print h h'):: parser_main t'
                    | _ -> raise(ParseError "Error! Invalid Type") 
                  end
            end
          (* Check with while loop*)
        | While_generic(_) ->
            begin 
              match t with 
                [] -> raise(ParseError "Error! Empty after while")
              | h'::t' -> 
                  begin
                    match h' with 
                      Condition_generic(_) ->
                        begin
                          match t' with
                            [] -> raise(ParseError "Error! Incomplete condition")
                          | h'' :: t'' ->
                              begin
                                match h'' with
                                  Token_generic(_)|Expression_generic(_)  ->
                                    (parser_while h' h''):: parser_main t''
                                | _ -> raise(ParseError "Error! Invalid type")
                              end
                        end
                    | _ -> raise(ParseError "Error! Invalid type")
            
                  end
            end    
          (*Check with if conditional statements*)  
        | If_generic(_) -> 
            begin
              match t with
                [] ->  raise(ParseError "Error! Empty after if")
              | h'::t' ->
                  begin
                    match h' with
                      Condition_generic(_) ->
                        begin
                          match t' with
                            [] -> raise(ParseError "Error! Incomplete condition")
                          | h''::t'' ->
                              begin
                                match h'' with
                                  Token_generic(_)|Expression_generic(_) ->
                                    begin
                                      match t'' with
                                        [] -> 
                                        (parser_only_if h' h''):: parser_main t''
                                      | h'''::t''' ->
                                          begin
                                            match h''' with
                                              Token_generic(_)|Expression_generic(_) ->
                                                (parser_if h' h'' h''') :: parser_main t'''
                                            | If_generic(_)|Print_generic(_)|While_generic(_) ->
                                                (parser_only_if h' h''):: parser_main t''
                                            | _ -> raise(ParseError "Error! Invalid Type")
                                          end
                                    end
                                | _ -> raise(ParseError "Error! Invalid Type")
                              end
                        end
                    | _ -> raise(ParseError "Error! Invalid Type")
                  end
            end
          (*Check with simple tokens *)
        | Token_generic(token) ->
            let binaryop = wrapper token in
              begin
                match binaryop with
                  Expression_generic(token') -> 
                    (Generic_expression(token'))::parser_main t
                | _ -> raise(ParseError "Error! Invalid type")
              end
          (*Check with other symbols*)
        | Others(_) ->
            parser_main t
          (*Check for invalid type*)
        | _ -> raise(ParseError "Error! Invalid Type")
      end  

            
let test_1 () = 
  (* let e = [Print_generic(Print_Lexer);Token_generic([Num_token(2);Operator(Multiply);Num_token(3);Operator(Add);Num_token(4)]);Print_generic(Print_Lexer);Token_generic([Num_token(2);Operator(Multiply);Num_token(3)])] in *)
  let e = [Print_generic(Print_Lexer);Token_generic([Num_token(2);Operator(Add)])] in
  parser_main e;;

let test_2 () = 
  (* let e = [Print_generic(Print_Lexer);Token_generic([Num_token(2);Operator(Multiply);Num_token(3);Operator(Add);Num_token(4)]);Print_generic(Print_Lexer);Token_generic([Num_token(2);Operator(Multiply);Num_token(3)])] in *)
  let e = [Print_generic(Print_Lexer);Token_generic([Num_token(2);Operator(Add);Num_token(3)])] in
  parser_main e;;

let test_3 () = 
  (* let e = [Print_generic(Print_Lexer);Token_generic([Num_token(2);Operator(Multiply);Num_token(3);Operator(Add);Num_token(4)]);Print_generic(Print_Lexer);Token_generic([Num_token(2);Operator(Multiply);Num_token(3)])] in *)
  let e = [Print_generic(Print_Lexer)] in
  parser_main e;;
 
(* let () = printreturn result   *)

(* test_1 (); *)
(* test_2 (); *)
test_3 ();

                            


      