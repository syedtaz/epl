type value = 
  | String of string
  | Number of int

module type Env = sig
  type t 
  val empty_env : unit -> t
  val extend_env : string -> value -> t -> t
  val extend_env' : string list -> value list -> t -> t
  val apply_env : string -> t -> value
  val is_empty_env : t -> bool
  val has_binding : string -> t -> bool
end

module RibcageEnv : Env = struct
  type t = (string list ref * value list ref) list
  exception VariableNotBound

  let empty_env = fun () -> []

  let extend_env vr vl env = match env with
    | [] -> let x = ref [vr] and y = ref [vl] in [(x, y)]
    | (vrs, vls) :: _ -> vrs := vr :: !vrs; vls := vl :: !vls; env

  let extend_env' vrs vls env = match env with
    | [] -> [(ref vrs, ref vls)]
    | _ :: _ -> (ref vrs, ref vls) :: env

  let rec apply_env_aux vrs vls vr = match (vrs, vls) with
    | ([], []) | ([], _) | (_, []) -> None
    | (vr' :: vrs', vl' :: vls') -> if vr = vr' then Some(vl') else apply_env_aux vrs' vls' vr

  let rec apply_env vr = function
    | [] -> raise VariableNotBound
    | (vrs, vls) :: t -> match apply_env_aux !vrs !vls vr with
      | None -> apply_env vr t
      | Some(vl) -> vl

  let is_empty_env = function
    | [] -> true
    | _ :: _ -> false

  let rec has_binding vr = function
    | [] -> false
    | (vrs, _) :: t -> List.mem vr !vrs || has_binding vr t
  
end

module AssocEnv : Env = struct
  type t = (string * value) list
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

module SymbolicEnv : Env = struct
  type t =
    | Empty
    | Extension of string * value * t
  exception VariableNotBound
  exception InvalidLengths

  let empty_env = fun () -> Empty

  let extend_env vr vl env = Extension(vr, vl, env)

  let rec extend_env' vrs vls env = match (vrs, vls) with
    | ([], []) -> env
    | (vr :: tvrs, vl :: tvls) -> extend_env' tvrs tvls (extend_env vr vl env)
    | ([], _ :: _) | (_ :: _, []) -> raise InvalidLengths

  let rec apply_env vr env = match env with
    | Empty -> raise VariableNotBound
    | Extension(vr', vl', env') -> if vr = vr' then vl' else apply_env vr env'

  let is_empty_env = function
    | Empty -> true
    | Extension(_) -> false

  let rec has_binding vr = function
    | Empty -> false
    | Extension(vr', _, env') -> vr = vr' || has_binding vr env'
end

module ProcEnv = struct
  type t = string -> value
  exception VariableNotBound
  let empty_env : t = fun _ -> raise VariableNotBound
  let apply_env (env : t) (vr : string) : value = env vr
  let extend_env (vr : string) (vl : value) (env : t) : t = 
    fun svr -> if svr = vr then vl else apply_env env svr
end

type lexpr = 
  | Term of string
  | Abstraction of string * lexpr
  | Application of lexpr * lexpr

module type LC = sig
  type t = lexpr
  type s = string
  val var_exp : s -> t
  val lambda_exp : s -> t -> t
  val app_exp : t -> t -> t
  val is_var_exp : t -> bool
  val is_lambda_exp : t -> bool
  val is_app_exp : t -> bool
  val var_of_var_exp : t -> s
  val bvar_of_lambda_exp : t -> s
  val body_of_lambda_exp : t -> t
  val rator_of_app_exp : t -> t
  val rand_of_app_exp : t -> t
end

module LambdaCalc (L : LC) = struct
  let rec occurs_free (sv : L.s) (t : L.t) : bool = 
    if L.is_var_exp t then sv = L.var_of_var_exp t else 
    (if L.is_lambda_exp t then (sv != L.bvar_of_lambda_exp t && occurs_free sv (L.body_of_lambda_exp t)) else
       (occurs_free sv (L.rator_of_app_exp t) || occurs_free sv (L.rand_of_app_exp t)))
end