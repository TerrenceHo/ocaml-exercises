module type ComplexSig = sig
  type t = float * float

  val zero : t
  val add : t -> t -> t
end

module Complex : ComplexSig = struct
  type t = float * float

  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)
end

module ListQueue = struct
  type 'a queue = 'a list

  let empty = []
  let is_empty q = q = []
  let enqueue x q = q @ [ x ]
  let peek = function [] -> None | h :: _ -> Some h
  let dequeue = function [] -> None | _ :: t -> Some t
end

module BatchListQueue = struct
  type 'a t = 'a list * 'a list

  let empty = ([], [])
  let is_empty = function [], [] -> true | _ -> false
  let shift = function inbox, [] -> ([], List.rev inbox) | q -> q
  let enqueue x (inbox, outbox) = shift (x :: inbox, outbox)
  let peek = function _, [] -> None | _, x :: _ -> Some x

  let dequeue = function
    | _, [] -> None
    | inbox, _ :: t -> Some (shift (inbox, t))
end

module type Map = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val lookup : 'k -> ('k, 'v) t -> 'v option
end

module BSTMap : Map = struct
  type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
  type ('k, 'v) t = ('k * 'v) tree

  let empty = Leaf

  let rec insert key value = function
    | Leaf -> Node ((key, value), Leaf, Leaf)
    | Node ((key', value'), ltree, rtree) ->
        if key = key' then Node ((key, value), Leaf, Leaf)
        else if key < key' then
          Node ((key', value'), insert key value ltree, rtree)
        else Node ((key', value'), ltree, insert key value rtree)

  let rec lookup k = function
    | Leaf -> None
    | Node ((k', v'), l, r) ->
        if k = k' then Some v' else if k < k' then lookup k l else lookup k r
end

(* fraction *)
module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  val make : int -> int -> t
  (** [make n d] is n/d. Requires d != 0. *)

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
end

module FractionPair : Fraction = struct
  type t = int * int

  let make numerator denominator = (numerator, denominator)
  let numerator (numerator, _) = numerator
  let denominator (_, denom) = denom
  let to_string (n, d) = string_of_int n ^ "/" ^ string_of_int d
  let to_float (n, d) = float_of_int n /. float_of_int d
  let add (n1, d1) (n2, d2) = ((n1 * d2) + (n2 * d1), d1 * d2)
  let mul (n1, d1) (n2, d2) = (n1 * n2, d1 * d2)
end

(* implementation of Euclid's gcd algorithm *)
let rec gcd x y =
  if x = 0 then y else if x < y then gcd (y - x) x else gcd y (x - y)

(* Functor that reduces a Fraction module into a fraction that is reduced *)
module ReducedFrac (F : Fraction) = struct
  let make n d =
    let v = F.make n d in
    let n' = F.numerator v in
    let d' = F.denominator v in
    let g = gcd (Int.abs n') (Int.abs d') in
    let n'', d'' = (n' / g, d' / g) in
    if d'' < 0 then (n'' * -1, d'' * -1) else (n'', d'')
end

(* ReducedFractionPair is the module created from the functor*)
module ReducedFractionPair = ReducedFrac (FractionPair)

let _ = assert (ReducedFractionPair.make 2 4 = (1, 2))
let _ = assert (ReducedFractionPair.make 3 4 = (3, 4))
let _ = assert (ReducedFractionPair.make 3 9 = (1, 3))
let _ = assert (ReducedFractionPair.make ~-3 9 = (~-1, 3))
let _ = assert (ReducedFractionPair.make ~-3 ~-9 = (1, 3))
let _ = assert (ReducedFractionPair.make 3 ~-9 = (~-1, 3))

(* dates *)
type date = { month : int; day : int }

module Date = struct
  type t = date

  let compare d1 d2 =
    if d1.month = d2.month then d1.day - d2.day else d1.month - d2.month
end

module DateMap = Map.Make (Date)

type calendar = string DateMap.t

let my_calendar =
  DateMap.(
    empty
    |> add { month = 2; day = 7 } "e day"
    |> add { month = 3; day = 14 } "pi day")

let print_calendar =
  DateMap.iter
    (fun date item -> Printf.printf "%d/%d %s\n" date.month date.day item)
    my_calendar

let thd (_, _, x) = x
let first_after m date = DateMap.(split date m |> thd |> min_binding |> snd)

(* char map *)
module CharMap = Map.Make (Char)

let is_for m = CharMap.mapi (fun k v -> Printf.sprintf "%c is for %s" k v) m

module CisString = struct
  type t = string

  let compare s1 s2 =
    String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)
end

module CisStringSet = Set.Make (CisString)

let _ =
  assert (
    CisStringSet.(equal (of_list [ "ABC"; "cde" ]) (of_list [ "abc"; "cdE" ])))

module type ToString = sig
  type t

  val to_string : t -> string
end

module Print (M : ToString) = struct
  let print v = print_string (M.to_string v)
end

module Int = struct
  type t = int

  let to_string = string_of_int
end

module PrintInt = Print (Int)

let _ = PrintInt.print 5

module MyString = struct
  type t = string

  let to_string s = s
end

module PrintString = Print (MyString)

let _ = PrintString.print "Hello\n"

module StringWithPrint = struct
  include String
  include PrintString
end

(* ring exercise *)

module type Ring = sig
  type t

  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
  val of_int : int -> t
end

module type Field = sig
  include Ring

  val ( / ) : t -> t -> t
end

module RingType (M : Ring) = struct
  type t
end
