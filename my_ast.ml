(*For Exception Handling*)
exception ParseError of string

(*Type Definitions*)
type operator = 
  Add
| Subtract
| Multiply
| Divide
| Assign  

type conditional_operator = 
  Equal
| Lessthan
| Greaterthan

type token =
  Operator of operator
| Num_token of int
| Id_token of string
| EOF 

type expression = 
  Num of int
| Id of string
| BinaryOp of expression * operator * expression

type condition = 
  expression * conditional_operator * expression

type my_print = 
  Print_Lex

type my_while = 
  While_Lex

type my_if = 
  If_Lex
  
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

type return_values = 
  Generic_print of my_print * generic_object
| Generic_while of condition * generic_object
| Generic_if of condition * generic_object * generic_object
| Generic_only_if of condition * generic_object
| Generic_expression of expression


(*************************************************************************************************)

(*FUNCTIONS BEGIN!*)


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
    let (binaryop, list) = parser_expression parser_list in
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
        | Expression_generic(_) ->
            Generic_print(token, t)
        | _ -> raise(ParseError "Error! Invalid type")  
      end 
  | _ -> raise(ParseError "Error! Invalid type") 
 

(* This is the parser function for while loop *) 
let parser_while cond exe = 
  match cond with
    Condition(token) ->
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
    Condition(token) -> 
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
    Condition(token) -> 
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
                      Condition(_) ->
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
          (*Check with if conditional statements*)  
        | If_generic(_) -> 
            begin
              match t with
                [] ->  raise("Error! Empty after if")
              | h'::t' ->
                  begin
                    match h' with
                      Condition(_) ->
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
                                                (parser_only_if h' h'' h''') :: parser_main t'''
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

            

                            


      