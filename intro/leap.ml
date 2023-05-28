let leap_year = function
  | year when year mod 400 = 0 -> true
  | year when year mod 100 = 0 -> false
  | year when year mod 4 = 0 -> true
  | _ -> false

let cond_str = function true -> "True!" | false -> "False!";;

print_endline "hello world";;
print_endline (cond_str (leap_year 400));;
print_endline (cond_str (leap_year 100));;
print_endline (cond_str (leap_year 4));;
print_endline (cond_str (leap_year 58))
