(* lexer.ml *)
open Str
open Types

let morphemsRegexDelimiters = "\\([0-9]+\\(\\.[0-9]+\\)?\\|[()+\\-*/]\\)"

let rec afficheStrList (lst:(string list)): unit =
  match lst with
  | [] -> ()
  | e::rst -> print_endline ("elem:" ^ e); afficheStrList rst

let afficheTokenList (tokens : token list) : unit =
  tokens
  |> List.map (function
      | TNombre e -> "TNombre=" ^ string_of_float e
      | TPlus -> "TPlus"
      | TMoins -> "TMoins"
      | TFois -> "TFois"
      | TDiv -> "TDiv"
      | TParG -> "TParG"
      | TParD -> "TParD")
  |> String.concat " "
  |> print_endline

let splitMorphems (s:string) : string list =
  let matches = Str.full_split (Str.regexp morphemsRegexDelimiters) s in
  List.rev (
    List.fold_left (fun acc e ->
      match e with
      | Str.Text t when t <> "" -> t :: acc
      | Str.Delim d -> d :: acc
      | _ -> acc
    ) [] matches
  )

let tokenize (s:string) : token list =
  let lst = splitMorphems s in
  List.rev (
    List.fold_left (fun acc e ->
      match float_of_string_opt e with
      | Some nb -> TNombre nb :: acc
      | None -> match e with
        | "(" -> TParG :: acc
        | ")" -> TParD :: acc
        | "+" -> TPlus :: acc
        | "-" -> TMoins :: acc
        | "*" -> TFois :: acc
        | "/" -> TDiv :: acc
        | _ -> failwith ("Token non reconnu : " ^ e)
    ) [] lst
  )
