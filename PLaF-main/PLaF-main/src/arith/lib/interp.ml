open Ds
open Parser_plaf.Ast
open Parser_plaf.Parser
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> int result =
  fun e ->
  match e with
  | Int n      -> return n
  | Add(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n+m)   
  | Sub(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n-m)   
  | Mul(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n*m)   
  | Div(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    if m=0
    then error "Division by zero"
    else return (n/m)
  | Abs(e) ->
    eval_expr e >>= fun n ->
    return (abs n)
  | Min(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (min n-m) 
  | IsZero(e) -> 
    eval_expr e >>= fun ev ->
    int_of_numVal ev >>= fun n ->
      return (BoolVal (n=0))
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
      return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    par_of_pairVal >>= fun (l,_) ->
      return l
  | Snd(e) ->
    eval_expr e >>=
    par_of_pairVal >>= fun (_,r) ->
      return r
  
  | Let(id,e1,e2) ->
    eval_expr e1 >>=
    extend_env id >>+
    eval_expr e2
  | Unpair(id1, id2, e1, e2) ->
    eval_expr e1 >>=
    pair_of_pairVal >>= fun (l,r) ->
      extend_env id1 l >>+
      extend_env id2 r >>+ 
      eval_expr e2
    
  |_->failwith"NO IMPT"

let par_of_pairVal ev =
  match ev with
  | PairVal(ev1,ev2) -> return (ev1,ev2)
  | _ -> error "Expected a pair"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : int result =
  e |> parse |> eval_prog

let int_of_numVal ev = 
  match ev with
  | numVal i -> return i 
  | _ -> error "Expected NUmber"

                