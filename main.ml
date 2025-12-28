(* main.ml *)
open Types
open Lexer
open Eval

let readUserInput () : string =
  print_endline "Entrer une expression : ";
  read_line ()

let () =
  let s = "3+2" in (*todo: spaces*)
  let tokens = tokenize s in
  afficheTokenList tokens;

  (* Ici, il faudra ajouter constructExpr pour créer l'arbre *)
  (* let ast = constructExpr tokens in *)
  (* print_float (eval ast) *)

