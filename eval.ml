open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec lookup env id=
match env with
  |[]->raise (DeclareError "lookup error")
  |(eid,ev)::t->if id=eid then ev
                else lookup t id

let rec pow ((x : int), (y : int)) : int =
 if y=0 then 1
 else if y > 0 then x * pow(x,y-1)
 else if (y mod (-2) = 0) then 0
 else -1

let rec eval_expr env t =
  match t with
  |Int i->Int_Val i
  |Bool i->Bool_Val i
  |ID id->lookup env id
  |Add (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Int_Val a,Int_Val b)->Int_Val (a+b)
                |_-> raise (TypeError "wrong")))
  |Sub (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Int_Val a,Int_Val b)->Int_Val (a-b)
                |_-> raise (TypeError "wrong")))
  |Mult (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Int_Val a,Int_Val b)->Int_Val (a*b)
                |_-> raise (TypeError "wrong")))
  |Div (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Int_Val a,Int_Val 0)->raise DivByZeroError
                |(Int_Val a,Int_Val b)->Int_Val (a/b)
                |_-> raise (TypeError "wrong")))
  |Pow (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Int_Val a,Int_Val b)->Int_Val (pow (a,b))
                |_-> raise (TypeError "wrong")))
  |Or (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Bool_Val a,Bool_Val b)->Bool_Val (a||b)
                |_-> raise (TypeError "wrong")))
  |And (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Bool_Val a,Bool_Val b)->Bool_Val (a&&b)
                |_-> raise (TypeError "wrong")))
  |Not e1->(let v1=eval_expr env e1 in
                (match v1 with  
                |Bool_Val true->Bool_Val (false)
                |Bool_Val false->Bool_Val (true)
                |_-> raise (TypeError "wrong")))
  |Greater (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Int_Val a,Int_Val b)->Bool_Val (a>b)
                |_-> raise (TypeError "wrong")))
  |Less (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Int_Val a,Int_Val b)->Bool_Val (a<b)
                |_-> raise (TypeError "wrong")))
  |GreaterEqual (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Int_Val a,Int_Val b)->Bool_Val (a>=b)
                |_-> raise (TypeError "wrong")))
  |LessEqual (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Int_Val a,Int_Val b)->Bool_Val (a<=b)
                |_-> raise (TypeError "wrong")))
  |Equal (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Int_Val a,Int_Val b)->Bool_Val (a=b)
                |(Bool_Val a,Bool_Val b)->Bool_Val (a=b)
                |_-> raise (TypeError "wrong")))
  |NotEqual (e1,e2)->(let v1=eval_expr env e1 and v2=eval_expr env e2 in
                (match (v1,v2) with  
                |(Int_Val a,Int_Val b)->Bool_Val (a!=b)
                |(Bool_Val a,Bool_Val b)->Bool_Val (a!=b)
                |_-> raise (TypeError "wrong")))


let rec update_id env rest id expr =
  match rest with
  |[]->raise (DeclareError "Haven't been declared")
  |(eid,ev)::t->if id=eid then let v=eval_expr env expr in
                    (match (v,ev) with
                    |(Int_Val a,Int_Val b)->((id,Int_Val a)::t)
                    |(Bool_Val a,Bool_Val b)->((id,Bool_Val a)::t)
                    |_-> raise (TypeError "wrong"))
                else (eid,ev)::(update_id env t id expr)

let rec declare_id env typ id=
  match env with
  |[]->(match typ with  
       |Int_Type->(id, Int_Val 0)
       |Bool_Type->(id, Bool_Val false))
  |(eid,ev)::t->if id=eid then raise (DeclareError "Have been declared")
                else declare_id t typ id


let rec eval_stmt env s =
  match s with
  |NoOp->env
  |Seq (s1, s2)-> eval_stmt (eval_stmt env s1) s2
  |Declare (typ,id)-> (declare_id env typ id)::env
  |Assign (id,expr)-> update_id env env id expr
  |If (expr,s1,s2)->(let condi = (eval_expr env expr) in 
                        match condi with 
                        |Int_Val i->raise (TypeError "wrong")
                        |Bool_Val b->if b then eval_stmt env s1 else eval_stmt env s2)
  |For (id,expr1,expr2,s)->(let start = eval_expr env expr1 and ending  = eval_expr env expr2 in 
                            (match (start,ending) with
                              |(Int_Val a,Int_Val b)->if a<=b then let newE = eval_stmt env (Assign (id,expr1)) in 
                                                                      let inc = Assign (id, Add(ID id,Int 1)) in
                                                                         let condi = (LessEqual (ID id,expr2)) in
                                                                    eval_stmt newE (While (condi, Seq (s,inc)))
                                                      else env
                              |_->raise (TypeError "wrong") ))
  |While (expr,s1)->(let condi = eval_expr env expr in 
                        (match condi with 
                        |Bool_Val false-> env
                        |Bool_Val true -> let newE = eval_stmt env s1 in eval_stmt newE (While (expr,s1))
                        |_->raise (TypeError "wrong") ))
  |Print expr->match eval_expr env expr with
               |Int_Val i->(let _=print_output_int i in let _=print_output_newline() in env)
               |Bool_Val b->(let _=print_output_bool b in let _=print_output_newline() in env)


      

  
  
  
  
                           
  
                                    


