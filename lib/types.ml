type addop = ADD | SUB
and mulop = MUL | DIV
and unop = NEG

and expr =
  | AddOp of expr * addop * term
  | ExprTerm of term

and term =
  | MulOp of term * mulop * factor
  | TermFactor of factor

and factor =
  | FactorUnary of unary

and unary =
  | UnaryOp of unop * unary
  | UnaryPrimary of primary

and primary =
  | Nombre of float
  | Groupe of expr

type token =
  | TNombre of float
  | TPlus
  | TMoins
  | TFois
  | TDiv
  | TParG
  | TParD
