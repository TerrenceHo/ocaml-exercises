(* list expressions *)
let l1 = [ 1; 2; 3; 4; 5 ]
let l2 = [ 1; 2; 3; 4; 5 ]
let l3 = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* product *)
let product l =
  let rec product_h l acc =
    match l with [] -> acc | h :: t -> (product_h [@tailcall]) t (acc * h)
  in
  product_h l 1

let _ = assert (product l1 = 120)
let _ = assert (product [] = 1)

(* concat *)
let concat l =
  let rec concat_h acc = function
    | [] -> acc
    | h :: t -> concat_h (acc ^ h) t
  in
  concat_h "" l

let _ = assert (concat [] = "")
let _ = assert (concat [ "1" ] = "1")
let _ = assert (concat [ "1"; "2"; "3" ] = "123")

(* pattern matching *)
let bigred = function [] -> false | h :: _ -> h = "bigred"
let _ = assert (bigred [] = false)
let _ = assert (bigred [ "bigred" ] = true)
let _ = assert (bigred [ "bigred"; "data" ] = true)
let _ = assert (bigred [ "data"; "bigred" ] = false)

let two_or_four = function
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

let _ = assert (two_or_four [ 1; 2 ] = true)
let _ = assert (two_or_four [ 1; 2; 3; 4 ] = true)
let _ = assert (two_or_four [ 1; 2; 3 ] = false)
let _ = assert (two_or_four [] = false)
let first_two_equal = function h1 :: h2 :: _ -> h1 = h2 | _ -> false
let _ = assert (first_two_equal [ 1; 2 ] = false)
let _ = assert (first_two_equal [ 1; 1 ] = true)
let _ = assert (first_two_equal [ 1; 1; 3 ] = true)
let _ = assert (first_two_equal [ 1; 2; 3 ] = false)
let _ = assert (first_two_equal [] = false)
let _ = assert (first_two_equal [ 1 ] = false)

(* list library *)
let fifth l = if List.length l < 5 then 0 else List.nth l 4
let sorted l = List.rev (List.sort Stdlib.compare l)
let sorted2 l = l |> List.sort Stdlib.compare |> List.rev
let last l = List.nth l (List.length l - 1)
let any_zeroes l = List.exists (fun x -> x = 0) l

(* take drop *)
(* shorthand to produce long lists *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)
let ( -- ) i j = from i j []

let take n lst =
  let rec take_h n acc l =
    if n = 0 then acc
    else
      match l with
      | [] -> acc
      | h :: t -> (take_h [@tailcall]) (n - 1) (h :: acc) t
  in
  let res = take_h n [] lst in
  List.rev res

let _ = assert (take 4 [ 1 ] = [ 1 ])
let _ = assert (take 4 [ 1; 2 ] = [ 1; 2 ])
let _ = assert (take 4 [ 1; 2; 3 ] = [ 1; 2; 3 ])
let _ = assert (take 4 [ 1; 2; 3; 4 ] = [ 1; 2; 3; 4 ])
let _ = assert (take 4 [ 1; 2; 3; 4; 5 ] = [ 1; 2; 3; 4 ])
let _ = assert (take 1000000 (1 -- 10000000) = 1 -- 1000000)

let rec drop n lst =
  if n = 0 then lst else match lst with [] -> [] | h :: t -> drop (n - 1) t

let _ = assert (drop 4 [ 1 ] = [])
let _ = assert (drop 4 [ 1; 2 ] = [])
let _ = assert (drop 4 [ 1; 2; 3 ] = [])
let _ = assert (drop 4 [ 1; 2; 3; 4 ] = [])
let _ = assert (drop 4 [ 1; 2; 3; 4; 5 ] = [ 5 ])
let _ = assert (drop 4 [ 1; 2; 3; 4; 5; 6 ] = [ 5; 6 ])
let _ = assert (drop 1000000 (1 -- 10000000) = 1000001 -- 10000000)

(* unimodal *)
let rec is_mon_dec = function
  | [] -> true
  | h :: [] -> true
  | h1 :: (h2 :: _ as t) -> if h1 < h2 then false else is_mon_dec t

let rec is_mon_inc_then_dec = function
  | [] -> true
  | h :: [] -> true
  | h1 :: (h2 :: _ as t) as lst ->
      if h1 <= h2 then is_mon_inc_then_dec t else is_mon_dec lst

(* powerset *)
let rec powerset s =
  match s with
  | [] -> [ [] ]
  | h :: t ->
      let set = powerset t in
      List.map (List.cons h) set @ set

(* print list *)
let rec print_int_list = function
  | [] -> ()
  | h :: t ->
      print_endline (string_of_int h);
      print_int_list t

let print_int_list' lst =
  List.iter (fun x -> print_endline (string_of_int x)) lst

(* student *)
type student = { first_name : string; last_name : string; gpa : float }

let s = { first_name = "Terrence"; last_name = "Ho"; gpa = 4.0 }
let full_name s = (s.first_name, s.last_name)
let new_student first_name last_name gpa = { first_name; last_name; gpa }

(* safe head tail *)
let safe_hd = function [] -> None | h :: _ -> Some h
let safe_tl = function [] -> None | _ :: t -> Some t

(* date before *)
type date = int * int * int

let is_before date1 date2 =
  let y1, m1, d1 = date1 in
  let y2, m2, d2 = date2 in
  y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)

(* tail recursive implementation that computes the earliest date with a helper, and *)
(* finds that if the list is empty return it as None *)
let earliest_date dates =
  let rec earliest_date_h earliest = function
    | [] -> Some earliest
    | h :: t ->
        if is_before h earliest then earliest_date_h h t
        else earliest_date_h earliest t
  in
  match dates with [] -> None | h :: t -> earliest_date_h h t

let dates =
  [
    (2023, 12, 1);
    (2024, 12, 1);
    (2022, 12, 1);
    (2022, 12, 1);
    (2022, 11, 1);
    (2022, 11, 2);
  ]

let _ = assert (earliest_date dates = Some (2022, 11, 1))
let _ = assert (earliest_date [] = None)

(* association lists or dictionaries *)
let insert k v lst = (k, v) :: lst

let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let num_assoc_list = insert 1 "one" (insert 2 "two" (insert 3 "three" []))
let _ = assert (lookup 2 num_assoc_list = Some "two")
let _ = assert (lookup 4 num_assoc_list = None)

(* cards *)
type suit = Diamonds | Clubs | Hearts | Spades

type rank =
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King

type card = { suit : suit; rank : rank }

let ace_of_clubs = { suit = Clubs; rank = Ace }
let queen_of_hearts = { suit = Hearts; rank = Queen }

(* quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign x = if x = 0 then Zero else if x < 0 then Neg else Pos

let quadrant (x, y) =
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Pos, Neg -> Some II
  | Neg, Neg -> Some III
  | Neg, Pos -> Some IV
  | _ -> None

let sign_poly x = if x < 0 then `Neg else if x = 0 then `Zero else `Pos

let quadrant_poly (x, y) =
  match (sign_poly x, sign_poly y) with
  | `Pos, `Pos -> Some I
  | `Pos, `Neg -> Some II
  | `Neg, `Neg -> Some III
  | `Neg, `Pos -> Some IV
  | _ -> None

(* tree stuff *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec depth = function
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)

let rec same_shape tree1 tree2 =
  match (tree1, tree2) with
  | Leaf, Leaf -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> same_shape l1 l2 && same_shape r1 r2
  | _ -> false

let inorder_list tree =
  let rec inorder_list_h tree acc =
    match tree with
    | Leaf -> acc
    | Node (x, l, r) ->
        let inorder_right = inorder_list_h r acc in
        let inorder_value = x :: inorder_right in
        inorder_list_h l inorder_value
  in
  inorder_list_h tree []

let is_bst tree =
  let rec is_ascending = function
    | [] -> true
    | [ h ] -> true
    | h1 :: (h2 :: t2 as t) -> if h1 < h2 then is_ascending t else false
  in
  is_ascending (inorder_list tree)

let t1 =
  Node
    ( 4,
      Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf)),
      Node (5, Node (6, Leaf, Leaf), Node (7, Leaf, Leaf)) )

let t2 =
  Node
    ( 4,
      Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf)),
      Node (6, Node (5, Leaf, Leaf), Node (7, Leaf, Leaf)) )

let t3 = Node (1, Leaf, Leaf)
let _ = assert (is_bst t1 = false)
let _ = assert (is_bst t2 = true)
let _ = assert (is_bst Leaf = true)
let _ = assert (is_bst t3 = true)
