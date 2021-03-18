(* Convert abstract syntax trees to generic trees of list of string *)

open Absyn

(* Helper functions *)

let name = Symbol.name
let map = List.map
let sprintf = Format.sprintf

(* Concatenate the lines of text in the node *)
let flat_nodes tree =
  Tree.map (String.concat "\n") tree

(* Build a singleton tree from a string *)
let mktr s = Tree.mkt [s]

(* Convert a symbol to a general tree *)
let tree_of_symbol s = mktr (Symbol.name s) []

(* Convert a binary operator to a string *)
let stringfy_operator op =
  match op with
  | Plus -> "+"
  | LT -> "<"

(* Convert an expression to a generic tree *)
let rec tree_of_exp exp =
  match exp with
  | IntExp x -> mktr (sprintf "IntExp %i" x) []
  | OpExp (op, l, r) -> mktr (sprintf "OpExp %s" (stringfy_operator op)) [tree_of_lexp l; tree_of_lexp r]

(* Convert an anotated expression to a generic tree *)
and tree_of_lexp (_, x) = tree_of_exp x

and tree_of_lsymbol (_, x) = tree_of_symbol x
