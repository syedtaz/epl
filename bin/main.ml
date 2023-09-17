let () = 
  List.iter (fun x -> Format.print_bool x) 
  (List.map (fun y -> Epl.Recursion.occurs_free "x" y) 
    [Term("x"); 
    Term("y"); 
    Abstraction("x", Application(Term("x"), Term("y")));
    Abstraction("y", Application(Term("x"), Term("y")))])