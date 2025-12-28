(* eval.ml *)
open Types

let rec eval (e: expr) : float =
  let rec eval_primary (p: primary) : float =
    match p with
    | Nombre n -> n
    | Groupe e -> eval e
  in
  let rec eval_unary (u: unary) : float =
    match u with
    | UnaryOp (NEG, u') -> -. (eval_unary u')
    | UnaryPrimary p -> eval_primary p
  in
  let rec eval_factor (f: factor) : float =
    match f with
    | FactorUnary u -> eval_unary u
  in
  let rec eval_term (t: term) : float =
    match t with
    | TermFactor f -> eval_factor f
    | MulOp (t1, MUL, f) -> eval_term t1 *. eval_factor f
    | MulOp (t1, DIV, f) -> eval_term t1 /. eval_factor f
  in
  match e with
  | ExprTerm t -> eval_term t
  | AddOp (e1, ADD, t) -> eval e1 +. eval_term t
  | AddOp (e1, SUB, t) -> eval e1 -. eval_term t
