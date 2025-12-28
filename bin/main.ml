
open Calculatrice.Lexer
open Calculatrice.Eval
open Calculatrice.Parser 

(*
let readUserInput () : string =
  print_endline "Entrer une expression : ";
  read_line ()
*) 

let () =
  let s = "-4*(-3+2)" in (*todo: spaces*)
  print_float (eval (parseTokens (tokenize s)))

  (* Ici, il faudra ajouter constructExpr pour créer l'arbre *)
  (* let ast = constructExpr tokens in *)
  (* print_float (eval ast) *)
