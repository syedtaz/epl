type expr = 
  | Integer of int
  | Boolean of bool

module State = struct
  let store = ref []

  let empty_store = fun () -> []
  let get_store = fun () -> store
  let initialize_store = fun () -> store := empty_store ()
  let reference = function
    | Integer _ -> true
    | Boolean _ -> false

  let deref n = List.nth !store n

  let newref vl = 
    let n = List.length !store in store := vl :: !store;
    n
end