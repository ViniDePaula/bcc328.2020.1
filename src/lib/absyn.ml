(* absyn.ml *)

type 'a loc = 'a Location.loc
  [@@deriving show]

type operator =
  | Plus
  | LT
  [@@deriving show]

type exp =
  | IntExp of int
  | OpExp of operator * lexp * lexp
  [@@deriving show]

and lexp = exp loc
  [@@deriving show]
