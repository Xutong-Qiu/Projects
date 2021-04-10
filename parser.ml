open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  let (remaining_tok,expr) = parse_Or toks in
    if remaining_tok = [EOF] then ([EOF],expr)
    else match (lookahead remaining_tok) with
        |Tok_Semi->(remaining_tok,expr)
        |Tok_RBrace->(remaining_tok,expr)
        |Tok_RParen->(remaining_tok,expr)
        |Tok_To->(remaining_tok,expr)
        |_-> raise (InvalidInputException "Error")

and parse_Or (toks : token list) : (token list * expr) =
  let (afterAnd_tok,expr) = parse_And toks in
    match (lookahead afterAnd_tok) with
    | Tok_Or ->let tok2 = match_token afterAnd_tok Tok_Or in
                    let (tok3, expr2) = parse_Or tok2 in
                        (tok3, Or (expr,expr2))
    |_->(afterAnd_tok,expr)

and parse_And (toks : token list) : (token list * expr) =
  let (afterE_tok,expr) = parse_E toks in
    match (lookahead afterE_tok) with
    | Tok_And ->let tok2 = match_token afterE_tok Tok_And in
                    let (tok3, expr2) = parse_And tok2 in
                        (tok3, And (expr,expr2))
    |_->(afterE_tok,expr)

and parse_E (toks : token list) : (token list * expr) =
  let (afterR_tok,expr) = parse_R toks in
    match (lookahead afterR_tok) with
    | Tok_Equal ->let tok2 = match_token afterR_tok Tok_Equal in
                    let (tok3, expr2) = parse_E tok2 in
                        (tok3, Equal (expr,expr2))
    | Tok_NotEqual ->let tok2 = match_token afterR_tok Tok_NotEqual in
                    let (tok3, expr2) = parse_E tok2 in
                        (tok3, NotEqual (expr,expr2))
    |_->(afterR_tok,expr)

and parse_R (toks : token list) : (token list * expr) =
  let (afterA_tok,expr) = parse_A toks in
    match (lookahead afterA_tok) with
    | Tok_Less ->let tok2 = match_token afterA_tok Tok_Less in
                    let (tok3, expr2) = parse_R tok2 in
                        (tok3, Less (expr,expr2))
    | Tok_Greater ->let tok2 = match_token afterA_tok Tok_Greater in
                    let (tok3, expr2) = parse_R tok2 in
                        (tok3, Greater (expr,expr2))
    | Tok_LessEqual ->let tok2 = match_token afterA_tok Tok_LessEqual in
                    let (tok3, expr2) = parse_R tok2 in
                        (tok3, LessEqual (expr,expr2))
    | Tok_GreaterEqual ->let tok2 = match_token afterA_tok Tok_GreaterEqual in
                    let (tok3, expr2) = parse_R tok2 in
                        (tok3, GreaterEqual (expr,expr2))
    |_->(afterA_tok,expr)

(* Parses the Add and sub *)
and parse_A (toks : token list) : (token list * expr) =
  let (afterM_tok,expr) = parse_M toks in
    match (lookahead afterM_tok) with
    | Tok_Add ->let tok2 = match_token afterM_tok Tok_Add in
                    let (tok3, expr2) = parse_A tok2 in
                        (tok3, Add (expr,expr2))
    | Tok_Sub ->let tok2 = match_token afterM_tok Tok_Sub in
                    let (tok3, expr2) = parse_A tok2 in
                        (tok3, Sub (expr,expr2))
    |_->(afterM_tok,expr)


(* Parses the Mult and Div. *)
and parse_M (toks : token list) : (token list * expr) =
  let (afterP_tok,expr) = parse_P toks in
    match (lookahead afterP_tok) with
    | Tok_Mult ->let tok2 = match_token afterP_tok Tok_Mult in
                    let (tok3, expr2) = parse_M tok2 in
                        (tok3, Mult (expr,expr2))
    | Tok_Div ->let tok2 = match_token afterP_tok Tok_Div in
                    let (tok3, expr2) = parse_M tok2 in
                        (tok3, Div (expr,expr2))
    |_->(afterP_tok,expr)


and parse_P (toks : token list) : (token list * expr) =
  let (afterU_tok,expr) = parse_U toks in
    match (lookahead afterU_tok) with
    | Tok_Pow ->let tok2 = match_token afterU_tok Tok_Pow in
                    let (tok3, expr2) = parse_P tok2 in
                        (tok3, Pow (expr,expr2))
    |_->(afterU_tok,expr)

and parse_U (toks : token list) : (token list * expr) =
  match (lookahead toks) with
  |Tok_Not ->let toks2=match_token toks (Tok_Not) in 
              let (toks3, expr2) = parse_U toks2 in
                (toks3, Not(expr2))
  |_-> parse_Pr toks
  

and parse_Pr (toks : token list) : (token list * expr) =
  match (lookahead toks) with
  |Tok_Bool i ->let toks2=match_token toks (Tok_Bool i) in 
                (toks2, Bool i)
  |Tok_Int i ->let toks2=match_token toks (Tok_Int i) in 
                (toks2, Int i)
  |Tok_ID i ->let toks2=match_token toks (Tok_ID i) in 
                (toks2, ID i)             
  |Tok_LParen ->let toks2=  match_token toks Tok_LParen in
                    let (toks3, expr2) = parse_expr toks2 in
                       let toks4=  match_token toks3 Tok_RParen in
                            (toks4,expr2)
  |_-> raise (InvalidInputException "Error")



let rec parse_stmt toks : stmt_result =
  if toks = [EOF] then ([EOF],NoOp)
  else if (lookahead toks) = Tok_RBrace then (toks, NoOp)
  else
    let (afterSO_tok,stmt) = parse_SO toks in
      let (afterST_tok,stmt1) = parse_stmt afterSO_tok in
         (afterST_tok, Seq (stmt,stmt1))
      

  and parse_SO (toks : token list) : stmt_result =
  match (lookahead toks) with
  |Tok_Int_Type->let toks2=match_token toks (Tok_Int_Type) in 
                  (match (lookahead toks2) with
                  |Tok_ID i ->let toks3 = match_token toks2 (Tok_ID i) in
                                let toks4 = match_token toks3 (Tok_Semi) in
                                  (toks4, Declare(Int_Type,i))
                  |_->raise (InvalidInputException "Error"))
  |Tok_Bool_Type->let toks2=match_token toks (Tok_Bool_Type) in 
                  (match (lookahead toks2) with
                  |Tok_ID i ->let toks3 = match_token toks2 (Tok_ID i) in
                                  let toks4 = match_token toks3 (Tok_Semi) in
                                    (toks4, Declare (Bool_Type,i))
                  |_->raise (InvalidInputException "Error"))
  |Tok_ID i-> let toks2=match_token toks (Tok_ID i) in 
                (match (lookahead toks2) with
                |Tok_Assign ->let toks3=match_token toks2 (Tok_Assign) in
                                let (afterPE_tok,expr) = parse_expr toks3 in
                                  let toks4 = match_token afterPE_tok (Tok_Semi) in
                                  (toks4, Assign (i,expr))
                |_->raise (InvalidInputException "Error"))
  |Tok_Print->let toks2=match_token toks (Tok_Print) in 
                (match (lookahead toks2) with
                |Tok_LParen ->let toks2=  match_token toks2 Tok_LParen in
                                let (toks3, expr) = parse_expr toks2 in
                                  let toks4=  match_token toks3 Tok_RParen in
                                    let toks4 = match_token toks4 (Tok_Semi) in
                                        (toks4,Print expr)
                |_->raise (InvalidInputException "Error"))
  |Tok_If->let toks2=match_token toks (Tok_If) in
                let toks2=  match_token toks2 Tok_LParen in
                                let (toks3, expr) = parse_expr toks2 in
                                  let toks4=  match_token toks3 Tok_RParen in
                                      let toks5=  match_token toks4 Tok_LBrace in
                                          let (toks6, stmt) = parse_stmt toks5 in
                                            let toks7=  match_token toks6 Tok_RBrace in
                                              (match (lookahead toks7) with
                                              |Tok_Else->
                                                        let toks8=  match_token toks7 Tok_Else in
                                                          let toks9=  match_token toks8 Tok_LBrace in
                                                            let (toks10, stmt1) = parse_stmt toks9 in
                                                              let toks11=  match_token toks10 Tok_RBrace in
                                                                  (toks11,If (expr,stmt,stmt1))
                                              |_->(toks7,If (expr,stmt,NoOp)))

   |Tok_For->let toks2=match_token toks (Tok_For) in
              let toks2=  match_token toks2 Tok_LParen in
                (match (lookahead toks2) with
                |Tok_ID i-> let toks3=  match_token toks2 (Tok_ID i) in
                              let toks4=match_token toks3 (Tok_From) in
                                let (toks5, expr) = parse_expr toks4 in
                                  let toks6=match_token toks5 (Tok_To) in
                                    let (toks7, expr1) = parse_expr toks6 in
                                      let toks8=match_token toks7 (Tok_RParen) in
                                        let toks9=  match_token toks8 Tok_LBrace in
                                            let (toks10, stmt) = parse_stmt toks9 in
                                              let toks11=  match_token toks10 Tok_RBrace in
                                                (toks11,For (i,expr,expr1,stmt))
                |_->raise (InvalidInputException "Error") )
    |Tok_While->let toks2=match_token toks (Tok_While) in
              let toks2=  match_token toks2 Tok_LParen in
                let (toks3, expr) = parse_expr toks2 in
                  let toks4=match_token toks3 (Tok_RParen) in
                    let toks5=  match_token toks4 Tok_LBrace in
                      let (toks6, stmt) = parse_stmt toks5 in
                        let toks7=  match_token toks6 Tok_RBrace in
                          (toks7,While (expr,stmt))
    |_->raise (InvalidInputException "Error")


let parse_main toks : stmt =
    let toks2=match_token toks (Tok_Int_Type) in 
      let toks3=match_token toks2 (Tok_Main) in 
        let toks3=  match_token toks3 Tok_LParen in
          let toks4=  match_token toks3 Tok_RParen in
            let toks5=  match_token toks4 Tok_LBrace in
              let (toks6, stmt) = parse_stmt toks5 in
                let toks6=  match_token toks6 Tok_RBrace in
                  let _=  match_token toks6 EOF in
                      stmt

