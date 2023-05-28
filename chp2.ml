(* Chp 2 exercises *)

let area r = Float.pi *. (r ** 2.)
let close_enough a b = Float.abs (a -. b) < 1e-5
let _ = assert (close_enough (area 1.0) Float.pi)
let rms x y = sqrt (((x *. x) +. (y *. y)) /. 2.)

let date_fun d m =
  match m with
  | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> d >= 1 && d <= 31
  | "Apr" | "Jun" | "Sept" | "Nov" -> d >= 1 && d <= 30
  | "Feb" -> d >= 1 && d <= 28
  | _ -> false

let _ = assert (date_fun 1 "Jan" = true)
let _ = assert (date_fun 32 "Jan" = false)

(* fib *)
let rec fib n = match n with 1 -> 1 | 2 -> 1 | n -> fib (n - 1) + fib (n - 2)
let _ = assert (fib 1 = 1)
let _ = assert (fib 2 = 1)
let _ = assert (fib 4 = 3)
let _ = assert (fib 10 = 55)

let fib_fast n =
  let rec fib_fast_h n p1 p2 =
    match n with 1 -> p1 | n -> fib_fast_h (n - 1) (p1 + p2) p1
  in
  fib_fast_h n 1 0

let _ = assert (fib_fast 1 = 1)
let _ = assert (fib_fast 2 = 1)
let _ = assert (fib_fast 4 = 3)
let _ = assert (fib_fast 10 = 55)
let _ = assert (fib_fast 50 = 12586269025)

(* divide *)
let divide ~numerator:x ~denominator:y = x /. y

(* average *)
let ( +/. ) x y = (x /. 2) +. (y /. 2)
