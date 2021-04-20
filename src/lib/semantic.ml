(* semantic.ml *)

open Absyn

let convertr (_, y) = y
let convertl (x, _) = x

let get_typeId (type_, id) = 
  match type_ with 
  | Absyn.Int -> (id, Absyn.Int)
  | Absyn.Bool -> (id, Absyn.Bool)

let rec get_types typeIds = 
  match typeIds with 
  | [typeId] -> 
    let (x, t) = get_typeId (typeId) in [t]
  | typeId :: rest -> 
    let (x1, t1) = get_typeId typeId in 
    let types = get_types rest in 
    t1 :: types

let get_fun (typeid, params, body) = 
  let (f, t0) = get_typeId typeid in 
  let paramsList = get_types params in
  let all = paramsList, t0 in
    (f, all)

let rec get_funs funs = 
  match funs with
  | [(loc, fundec)] -> 
    let (f, t) = get_fun fundec in 
    Symbol.enter f t Symbol.empty
  | fundec :: rest -> 
    let (f, t) = get_fun (convertr fundec) in 
    let table = get_funs rest in 
    match Symbol.look f table with 
    | Some _ -> Error.error (Location.loc fundec) "function %s defined more than once" (Symbol.name f)
    | None -> Symbol.enter f t table 

let rec check_typeids (typeIds) (f) (body) = 
  match typeIds with 
  | [typeId] -> 
    let (x, t) = get_typeId typeId in
    Symbol.enter x t Symbol.empty
  | typeId :: rest -> 
    let (x, t) = get_typeId typeId in 
    let vtable = check_typeids rest f body in 
    match Symbol.look x vtable with 
    | Some _ -> Error.error (Location.loc body) "typeId %s defined more than once" (Symbol.name x)
    | None -> Symbol.enter x t vtable

let rec check_type_list list1 list2 loc = 
  match list1,list2 with 
  | [item1], [item2] -> 
    if item1 == item2 then () 
      else 
        Error.error (Location.loc loc) "Param type not match"
  | item1 :: rest1, item2 :: rest2 -> 
    if item1 == item2 then
      let x = check_type_list (rest1) (rest2) in
      ()
    else 
      Error.error (Location.loc loc) "Param type not match"

let rec check_exp (lexp) (vtable) (ftable) =
  match lexp with 
  | (_ , Absyn.IntExp _)  -> Absyn.Int 

  | (_, Absyn.IdExp x) -> 
    (match Symbol.look x vtable with
    | Some res -> res
    | None -> Error.error (Location.loc lexp) "typeId %s not found in vtable" (Symbol.name x))

  | (_, Absyn.OpExp (op, l, r)) -> 
      let t1 = check_exp l vtable ftable in
      let t2 = check_exp r vtable ftable in
      (match op with 
      | Plus -> 
      if t1 = Absyn.Int && t2 = Absyn.Int
        then Absyn.Int
    else 
      Error.error (Location.loc lexp) "Sum exp not valid"
      | LT -> 
        if t1 == t2 
          then Absyn.Bool
        else 
          Error.error (Location.loc lexp) "LT exp not valid")

  | (_, Absyn.IfExp (x, y, z)) -> 
    let t1 = check_exp x vtable ftable in 
    let t2 = check_exp y vtable ftable in 
    let t3 = check_exp z vtable ftable in 
    if t1 == Absyn.Bool && t2 == t3 
      then t2 
    else 
      Error.error (Location.loc lexp) "IfExp not valid"

  | (_, Absyn.FuncExp (x, y)) -> 
    (match Symbol.look x ftable with
    | Some (typeList, returnType) -> 
      let listTypes = check_exps y vtable ftable in
      let lengthList = List.length listTypes in
      let lengthOriginal = List.length typeList in 
      if (lengthList == lengthOriginal)
        then 
          let typeCheck = check_type_list typeList listTypes lexp in
          returnType;
      else
        Error.error (Location.loc lexp) "Invalid params size"
    | None -> Error.error (Location.loc lexp) "Function %s not found in ftable" (Symbol.name x))

  | (_, Absyn.DeclExp (x, f, ff)) -> 
    let t1 = check_exp f vtable ftable in 
    let vtable' = Symbol.enter x t1 vtable in 
    check_exp ff vtable' ftable;

and check_exps (exps) (vtable) (ftable) = 
  match exps with 
  | [exp] -> let t = check_exp exp vtable ftable in
        [t]
  | exp :: rest -> 
    check_exp exp vtable ftable ::
    check_exps rest vtable ftable

let check_fun (typeid, params, body) (ftable) = 
  let (f, t0) = get_typeId typeid in 
  let vtable = check_typeids params f body in 
  let t1 = check_exp body vtable ftable in
  if t0 != t1 then 
    Error.error (Location.loc body) "Invalid return exp %s" (Symbol.name f) 

let rec check_funs funs ftable = 
  match funs with 
  | [(loc, fundec)] -> check_fun (fundec) ftable
  | fundec :: rest -> 
    check_fun (convertr fundec) ftable;
    check_funs rest ftable

let check_program (loc, funs) = 
  match funs with
  | FunsList (x) ->
    let ftable = get_funs x in  
    check_funs x ftable;
    match Symbol.look (Symbol.symbol "main") ftable with
    | Some (typeList, res) -> 
      if res != Absyn.Int
        then
          Error.error (loc) "Main function must be type Int"
      else
        ()
    | None _ -> Error.error (loc) "Main function not found"

