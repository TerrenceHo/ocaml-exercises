let rec fast_exponentiation n r =
  match r with
  | 0 -> 1
  | 1 -> n
  | _ -> (
      match r mod 2 = 0 with
      | true -> fast_exponentiation (n * n) (r / 2)
      | false -> fast_exponentiation (n * n) (r / 2) * n)
;;

print_int (fast_exponentiation 2 10);;
print_endline ""
