type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

let int_of_day d =
  match d with
  | Sun -> 1
  | Mon -> 2
  | Tue -> 3
  | Wed -> 4
  | Thu -> 5
  | Fri -> 6
  | Sat -> 7

let rec sum = function [] -> 0 | x :: xs -> x + sum xs

(* list expressions *)
let list1 = [ 1; 2; 3; 4; 5 ]
let list2 = [ 1; 2; 3; 4; 5 ] (* used :: and [], but the formatter removed it *)
let list3 = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* product *)
let rec product = function [] -> 1 | h :: t -> h * product t

(* concat *)
let rec concat = function [] -> "" | h :: t -> h ^ concat t

(* patterns *)
let bigred = function h :: _ -> h = "bigred" | _ -> false

let two_four_eles = function
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

let first_two_equal = function h1 :: h2 :: _ -> h1 = h2 | _ -> false

(* library test *)
let fifth_ele lst = if List.length lst < 5 then 0 else List.nth lst 4
let sort_list_descending lst = List.rev (List.sort Stdlib.compare lst)

(* uses partial application *)
let sort_list_descending' lst = lst |> List.sort Stdlib.compare |> List.rev

let rec take n lst =
  if n = 0 then []
  else match lst with [] -> [] | h :: t -> h :: take (n - 1) t

let rec take_tr' n lst acc =
  if n = 0 then acc
  else match lst with [] -> acc | h :: t -> take_tr' (n - 1) t (h :: acc)

let take_tr n lst = take_tr' n lst [] |> List.rev

let rec drop n lst =
  if n = 0 then lst else match lst with [] -> [] | _ :: t -> drop (n - 1) t

let rec from' i j acc = if i > j then acc else from' i (j - 1) (j :: acc)
let from i j = from' i j []
let ( -- ) i j = from' i j []
