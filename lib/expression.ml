type exp = 
  | Const of int
(* Bools *)
  | Zero of exp
  | Equal of exp * exp
  | Greater of exp * exp
  | Lesser of exp * exp
  | Minus of exp
  | If of exp * exp * exp
(* Arithmetic *)
  | Diff of exp * exp
  | Add of exp * exp
  | Mul of exp * exp
  | Div of exp * exp
(* Lists *)
  | Emptylist
  | Cons of exp * exp
  | Car of exp
  | Cdr of exp
  | Empty of exp
(* Binding *)
  | Var of string
  | Let of string * exp * exp

type exp_val = 
  | Int of int
  | Bool of bool
  | List of cons
and cons =
  | Nil
  | Cons of exp_val * cons

  module Value = struct
    exception NotNumeric
    exception NotBoolean
    exception NotEmpty
    exception NotList
    let nval n = Int n
    let bval b = Bool b
    let lval v = List v
    let cons a lst = Cons(a, lst)
    let car lst = match lst with
      | Nil -> raise NotEmpty
      | Cons(a, _) -> a
    let cdr lst = match lst with
      | Nil -> raise NotEmpty
      | Cons(_, b) -> b
    let num_of_exp = function
      | Int n -> n
      | Bool _ | List _ -> raise NotNumeric
    let bool_of_exp = function
      | Bool b -> b
      | Int _ | List _ -> raise NotBoolean
    let lst_of_exp = function
      | List l -> l
      | Int _ | Bool _ -> raise NotList
  end

module Environment  = struct
  type t = (string * exp_val) list
  exception VariableNotBound
  exception InvalidLengths

  let empty_env = fun () -> []

  let extend_env vr vl env = (vr, vl) :: env

  let rec extend_env' vrs vls env = match (vrs, vls) with
    | ([], []) -> env
    | (vr :: tvrs, vl :: tvls) -> extend_env' tvrs tvls (extend_env vr vl env)
    | ([], _ :: _) | (_ :: _, []) -> raise InvalidLengths

  let rec apply_env vr = function
    | [] -> raise VariableNotBound
    | (vr', vl) :: t -> if vr = vr' then vl else apply_env vr t

  let is_empty_env = function
    | [] -> true
    | _ :: _ -> false

  let rec has_binding vr = function
    | [] -> false
    | (vr', _) :: t -> vr = vr' || has_binding vr t
end

let nchain e1 e2 env f op = 
  let l = Value.num_of_exp (f e1 env)
  and r = Value.num_of_exp (f e2 env) in
  Value.nval (op l r)

let bchain e1 e2 env f op = 
  let l = Value.num_of_exp (f e1 env)
  and r = Value.num_of_exp (f e2 env) in
  Value.bval (op l r)

exception Division_by_zero

let rec value_of (e : exp) (env : Environment.t) = match e with
  | Const(n) -> Value.nval n
  | Var(vr) -> Environment.apply_env vr env
  | Minus(e') -> Value.nval (- Value.num_of_exp (value_of e' env))
  | Equal(e1, e2) -> bchain e1 e2 env value_of ( = )
  | Greater(e1, e2) -> bchain e1 e2 env value_of ( > )
  | Lesser(e1, e2) -> bchain e1 e2 env value_of ( < )
  | Add(l, r) -> nchain l r env value_of ( + )
  | Mul(l, r) -> nchain l r env value_of ( * )
  | Diff(l, r) -> nchain l r env value_of ( - )
  | Div(l, r) -> (
    let l' = Value.num_of_exp (value_of l env) 
    and r' = Value.num_of_exp (value_of r env) in
    match r' with
      | 0 -> raise Division_by_zero
      | _ ->  Value.nval (l' + r'))
  | Zero(e') -> (
      let f = value_of e' env in 
      match Value.num_of_exp f with
        | 0 -> Bool(true)
        | _ -> Bool(false))
  | If(e1, e2, e3) -> (
      let f = value_of e1 env in
      match Value.bool_of_exp f with
        | true -> value_of e2 env
        | false -> value_of e3 env)
  | Let(vr, e', body) -> (
      let f = value_of e' env in
      value_of body (Environment.extend_env vr f env))
  | Cons(e1, e2) -> (
      let f = value_of e1 env 
      and g = value_of e2 env in
      let l = Value.lst_of_exp g in
      Value.cons f l |> Value.lval)
  | Car(e') -> value_of e' env |> Value.lst_of_exp |> Value.car
  | Cdr(e') -> value_of e' env |> Value.lst_of_exp |> Value.cdr |> Value.lval
  | Emptylist -> Nil |> Value.lval
  | Empty(e') -> (match value_of e' env |> Value.lst_of_exp with
    | Nil -> Value.bval true
    | Cons _ -> Value.bval false)
