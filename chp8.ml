(* Red Black Tree *)
type color = Red | Black
type 'a rbtree = Leaf | Node of color * 'a * 'a rbtree * 'a rbtree

let rec mem x = function
  | Leaf -> false
  | Node (_, y, l, r) ->
      if x < y then mem x l else if x > y then mem x r else true

let balance = function
  | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | a, b, c, d -> Node (a, b, c, d)

let insert x s =
  let rec ins = function
    | Leaf -> Node (Red, x, Leaf, Leaf)
    | Node (color, y, a, b) as s ->
        if x < y then balance (color, y, ins a, b)
        else if x > y then balance (color, y, a, ins b)
        else s
  in
  match ins s with
  | Node (_, y, a, b) -> Node (Black, y, a, b)
  | Leaf ->
      (* guaranteed to be non-empty *)
      failwith "RBT insert failed with ins returning leaf"

(* Sequences *)
let rec ones = 1 :: ones

let rec a = 0 :: b
and b = 1 :: a

(* take in a function so that the ADT is lazily evaluated *)
type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec from n = Cons (n, fun () -> from (n + 1))
let nats = from 0
let hd (Cons (h, _)) = h
let tl (Cons (_, t)) = t ()
let rec take n s = if n = 0 then [] else hd s :: take (n - 1) (tl s)
let rec drop n s = if n = 0 then s else drop (n - 1) (tl s)

(* laziness *)
let rec fibs = Cons (1, fun () -> Cons (1, fun () -> sum fibs (tl fibs)))
