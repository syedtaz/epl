let listlength lst = 
  let rec aux acc = function
    | [] -> acc
    | _ :: t -> aux (acc + 1) t
in aux 0 lst

let rec nth n = function
  | [] -> None
  | h :: t -> if n = 0 then Some(h) else nth (n - 1) t

let remove_first lst v = 
  let rec aux lst v acc = match lst with
  | h :: t when h = v -> Some(t @ acc)
  | h :: t -> aux t v (h :: acc)
  | [] -> None
in match aux lst v [] with
  | Some(res) -> List.rev res
  | None -> lst

let rec replace a b acc = function
  | [] -> List.rev acc
  | h :: t -> if h = a then replace a b (b :: acc) t else replace a b (h :: acc) t

let rec subst a b lst = match lst with
  | h :: t -> [(replace a b [] h)] @ subst a b t
  | [] -> []

type lexpr = 
  | Term of string
  | Abstraction of string * lexpr
  | Application of lexpr * lexpr

let rec occurs_free v = function
  | Term(t) -> v = t
  | Abstraction(b, t) -> v != b && occurs_free v t
  | Application(l, r) -> occurs_free v l || occurs_free v r

