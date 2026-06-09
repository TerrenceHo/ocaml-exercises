(** Exercise: values *)

(* int  *)
let _ = 7 * (1 + 2 + 3)

(* string *)
let _ = "CS " ^ string_of_int 3110

(* * Exercise: operators *)
let _ = 42 * 10
let _ = 3.14 /. 2.0
let _ = 4.2 ** 7.0

(* Equality *)
let _ = 42 = 42
let _ = "hi" = "hi"
let _ = "hi" == "hi"

(* if  *)
let _ = if 2 > 1 then 42 else 7

(* double *)
let double x = x * 2
let _ = assert (double 2 = 4)
let cube x = x *. x *. x
let _ = assert (cube 0. = 0.)
let _ = assert (cube 1. = 1.)
let _ = assert (cube 3. = 27.)
let sign x = if x < 0 then -1 else if x > 0 then 1 else 0
let area r = Float.pi *. (r ** 2.)
let rms x y = Float.sqrt (((x *. x) +. (y *. y)) /. 2.)

let date d m =
  match m with
  | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> 1 <= d && d <= 31
  | "Apr" | "Jun" | "Sept" | "Nov" -> 1 <= d && d <= 30
  | "Feb" -> 1 <= d && d <= 28
  | _ -> false

let rec fib n =
  if n = 1 then 1 else if n = 2 then 1 else fib (n - 1) + fib (n - 2)

let rec fib_fast_helper n p1 p2 =
  if n = 0 then 0 else fib_fast_helper (n - 1) p2 (p1 + p2)

let fib_fast n = fib_fast_helper n 0 1
let divide num denom = num /. denom
let add x y = x + y
let ( +/. ) a b = (a +. b) /. 2.0
let _ = assert (1.0 +/. 2.0 = 1.5)
let _ = assert (0. +/. 0. = 0.)
