
open Calculatrice.Lexer
open Calculatrice.Eval
open Calculatrice.Parser 


let readUserInput () : string =
  print_endline "Entrer une expression : ";
  read_line ()

let doACalculation () : unit = 
  let s = (readUserInput ()) in 
  print_float (eval (parseTokens (tokenize s)))

let () =
  doACalculation ()
