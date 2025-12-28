open Types

let parseTokens (tokens : token list) : expr = 
  let rec parse_primary (tokens : token list) : (primary * token list) = 
    match tokens with 
    | TNombre n :: rest -> (Nombre n, rest)
    | TParG :: rest ->
        let (e, rest_after_expr) = parse_expr rest in 
        (match rest_after_expr with
         | TParD :: rest_final -> (Groupe e, rest_final)
         | _ -> failwith "Erreur : parenthèse fermante ')' attendue")
    | _ -> failwith "Erreur : nombre ou '(' attendu"
  
  and parse_unary (tokens : token list) : (unary * token list) = 
    match tokens with 
    | TMoins :: rest -> 
        let (u, rest_final) = parse_unary rest in
        (UnaryOp (NEG, u), rest_final)
    | _ -> 
        let (p, rest) = parse_primary tokens in
        (UnaryPrimary p, rest)
  
  and parse_factor (tokens : token list) : (factor * token list) =
    let (u, rest) = parse_unary tokens in
    (FactorUnary u, rest)
  
  and parse_term (tokens : token list) : (term * token list) =
    let (f, rest) = parse_factor tokens in
    
    let rec parse_term_rest (left : term) (tokens : token list) : (term * token list) =
      match tokens with
      | TFois :: rest' ->
          let (f, rest'') = parse_factor rest' in
          parse_term_rest (MulOp (left, MUL, f)) rest''
      
      | TDiv :: rest' ->
          let (f, rest'') = parse_factor rest' in
          parse_term_rest (MulOp (left, DIV, f)) rest''
      
      | _ ->
          (left, tokens)
    in
    parse_term_rest (TermFactor f) rest
  
  and parse_expr (tokens : token list) : (expr * token list) = 
    let (t, rest) = parse_term tokens in
    
    let rec parse_expr_rest (left : expr) (tokens : token list) : (expr * token list) =
      match tokens with
      | TPlus :: rest' ->
          let (t, rest'') = parse_term rest' in
          parse_expr_rest (AddOp (left, ADD, t)) rest''
      
      | TMoins :: rest' ->
          let (t, rest'') = parse_term rest' in
          parse_expr_rest (AddOp (left, SUB, t)) rest''
      
      | _ ->
          (left, tokens)
    in
    parse_expr_rest (ExprTerm t) rest
  
  in
  
  let (e, rest) = parse_expr tokens in
  match rest with
  | [] -> e
  | _ -> failwith "Erreur : tokens restants après parsing"