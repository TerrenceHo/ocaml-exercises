let rec from i j l = if i > j then l else from i (j - 1) (j :: l)
let ( -- ) i j = from i j []

let rec combine f init = function
  | [] -> init
  | h :: t -> f h (combine f init t)

let sum = combine ( + ) 0
let concat = combine ( ^ ) ""

(* repeat *)
let rec repeat f n x = if n = 0 then x else repeat f (n - 1) (f x)

(* product *)
let product_left lst = List.fold_left ( *. ) 1.0 lst
let product_right lst = List.fold_right ( *. ) lst 1.0

(* sum_cube_odd *)
let sum_cube_odd n =
  List.fold_left ( + ) 0
    (List.map
       (fun x -> x * x * x)
       (List.filter (fun x -> x mod 2 = 1) (1 -- n)))

let sum_cube_odd_pipeline n =
  1 -- n
  |> List.filter (fun x -> x mod 2 = 1)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( + ) 0

let _ = assert (sum_cube_odd 3 = 28)
let _ = assert (sum_cube_odd_pipeline 3 = 28)

(* exists *)
let rec exists_rec p = function [] -> false | h :: t -> p h || exists_rec p t
let exists_fold p lst = List.fold_left (fun acc ele -> acc || p ele) false lst
let exists_lib = List.exists

(* map composition *)
(* List.map f (List.map g lst) *)
(* List.map (fun x -> f (g x)) lst *)

(* more list fun *)
let strings_gt_3 = List.filter (fun x -> String.length x > 3)

let _ =
  assert (
    strings_gt_3 [ "1"; "22"; "333"; "4444"; "55555" ] = [ "4444"; "55555" ])

let join_with strs sep =
  match strs with
  | [] -> ""
  | h :: t -> h ^ List.fold_left (fun combined v -> combined ^ sep ^ v) h t

(* valid matrix *)
let is_valid_matrix = function
  | [] -> false
  | h :: t ->
      let l = List.length h in
      l > 0 && List.for_all (fun x -> List.length x = l) t

(* matrix ops *)
let add_row_vectors = List.map2 ( + )
let add_matrices = List.map2 add_row_vectors
