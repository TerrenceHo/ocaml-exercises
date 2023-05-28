let rec list_max l =
  match l with
  | [] -> None
  | h :: t -> (
      match list_max t with None -> Some h | Some m -> Some (max h m))

let print_max data =
  match data with
  | None -> print_endline "list empty!"
  | Some h ->
      print_int h;
      print_endline ""
;;

print_max (list_max [ 1; 2; 3; 4; 5 ]);
print_max (list_max [])
