(* absyn.ml *)

type symbol = Symbol.symbol
  [@@deriving show]

type 'a loc = 'a Location.loc
  [@@deriving show]

type operator =
  | Plus
  | LT
  [@@deriving show]

type exp =
  | IntExp  of int
  | OpExp   of operator * lexp * lexp
  | IdExp   of symbol
  | IfExp   of lexp * lexp * lexp 
  | FuncExp of symbol * (lexp) list
  | DeclExp of symbol * lexp * lexp 
  [@@deriving show]

and fundec = (type_ * symbol) * (type_ * symbol) list * lexp
  [@@deriving show]

and funs = 
  | FunsList of (lfundec) list 
  [@@deriving show]

and type_ =
  | Int
  | Bool
  [@@deriving show]

and lexp = exp loc
  [@@deriving show]

and lfundec = fundec loc
  [@@deriving show]

and lfuns = funs loc 
  [@@deriving show]

