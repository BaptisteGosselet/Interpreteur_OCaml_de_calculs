
open Calculatrice.Lexer
open Calculatrice.Eval
open Calculatrice.Parser 


let readUserInput () : string =
  print_endline "Entrer une expression ('q' pour quitter): ";
  read_line ()

let rec doACalculation () =
  let s = readUserInput () in
  let result = eval (parseTokens (tokenize s)) in
  Printf.printf "> %.15g\n%!" result;
  doACalculation ()


let () =
  doACalculation ()
