open TokenTypes


let re_sub = Str.regexp "-"
let re_semi = Str.regexp ";"
let re_rparen = Str.regexp ")"
let re_rbrace = Str.regexp "}"
let re_print = Str.regexp "printf"
let re_pow = Str.regexp "\\^"
let re_add = Str.regexp "\\+"
let re_or = Str.regexp "||"
let re_nequal = Str.regexp "!="
let re_not = Str.regexp "!"
let re_mult = Str.regexp "\\*"
let re_main = Str.regexp "main"
let re_lequal = Str.regexp "<="
let re_less = Str.regexp "<"
let re_lparen = Str.regexp "("
let re_lbrace = Str.regexp "{"
let re_int = Str.regexp "-?[0-9]+"
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_gequal = Str.regexp ">="
let re_greater = Str.regexp ">"
let re_equal = Str.regexp "=="
let re_div = Str.regexp "/"
let re_bool = Str.regexp "true\\|false"
let re_assign = Str.regexp "="
let re_and = Str.regexp "&&"


let rec tok pos s =
    if pos >= String.length s then [EOF]
    else if (Str.string_match re_lparen s pos) then
      (Tok_LParen)::(tok (pos+ 1) s) 
    else if (Str.string_match re_rparen s pos) then
      (Tok_RParen)::(tok (pos+1) s)
    else if (Str.string_match re_lbrace s pos) then
      (Tok_LBrace)::(tok (pos+ 1) s) 
    else if (Str.string_match re_rbrace s pos) then
      (Tok_RBrace)::(tok (pos+ 1) s)
    else if (Str.string_match re_int s pos) then
      let token = Str.matched_string s in
      (Tok_Int ((int_of_string)token))::(tok (pos+ (String.length token)) s) 
    else if (Str.string_match re_add s pos) then
      (Tok_Add)::(tok (pos+ 1) s)  
    else if (Str.string_match re_or s pos) then
      (Tok_Or)::(tok (pos+ 2) s)   
    else if (Str.string_match re_nequal s pos) then
      (Tok_NotEqual)::(tok (pos+ 2) s)   
    else if (Str.string_match re_not s pos) then
      (Tok_Not)::(tok (pos+ 1) s)  
    else if (Str.string_match re_mult s pos) then
      (Tok_Mult)::(tok (pos+ 1) s)
    else if (Str.string_match re_lequal s pos) then
      (Tok_LessEqual)::(tok (pos+ 2) s) 
    else if (Str.string_match re_less s pos) then
      (Tok_Less)::(tok (pos+ 1) s)
    else if (Str.string_match re_sub s pos) then
      (Tok_Sub)::(tok (pos+ 1) s)
    else if (Str.string_match re_semi s pos) then
      (Tok_Semi)::(tok (pos+ 1) s)
    else if (Str.string_match re_pow s pos) then
      (Tok_Pow)::(tok (pos+ 1) s)
    else if (Str.string_match re_gequal s pos) then
      (Tok_GreaterEqual)::(tok (pos+ 2) s) 
    else if (Str.string_match re_greater s pos) then
      (Tok_Greater)::(tok (pos+ 1) s) 
    else if (Str.string_match re_equal s pos) then
      (Tok_Equal)::(tok (pos+ 2) s) 
    else if (Str.string_match re_div s pos) then
      (Tok_Div)::(tok (pos+ 1) s) 
    else if (Str.string_match re_assign s pos) then
      (Tok_Assign)::(tok (pos+ 1) s)
    else if (Str.string_match re_and s pos) then
      (Tok_And)::(tok (pos+ 2) s) 
    else if (Str.string_match re_id s pos) then
      let token = Str.matched_string s in
      if token = "for" then (Tok_For)::(tok (pos+ 3) s)
      else if token = "from" then (Tok_From)::(tok (pos+ 4) s)
      else if token = "to" then (Tok_To)::(tok (pos+ 2) s)
      else if token = "while" then (Tok_While)::(tok (pos+ 5) s)
      else if token = "int" then (Tok_Int_Type)::(tok (pos+ 3) s)  
      else if token = "printf" then (Tok_Print)::(tok (pos+ 6) s)
      else if token = "main" then (Tok_Main)::(tok (pos+ 4) s) 
      else if token = "if" then (Tok_If)::(tok (pos+ 2) s) 
      else if token = "else" then (Tok_Else)::(tok (pos+ 4) s) 
      else if token = "bool" then (Tok_Bool_Type)::(tok (pos+ 4) s) 
      else if token = "true" then (Tok_Bool ((bool_of_string)token))::(tok (pos+ 4) s) 
      else if token = "false" then (Tok_Bool ((bool_of_string)token))::(tok (pos+ 5) s) 
      else (Tok_ID (token))::(tok (pos+ (String.length token)) s) 

    else tok (pos+1) s
        

let tokenize input =
    tok 0 input
