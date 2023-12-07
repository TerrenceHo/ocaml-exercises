module LinkedList = struct
  type 'a node = { next : 'a mlist; value : 'a ref }
  and 'a mlist = 'a node option ref

  (* empty requires invalid argument to indicate that it is not just a value, *)
  (* otherwise there would only ever be one empty list that is created *)
  let empty () : 'a mlist = ref None

  let insert_first (lst : 'a mlist) (v : 'a) : unit =
    lst := Some { next = ref !lst; value = ref v }

  let rec set_value (lst : 'a mlist) (k : int) (v : 'a) : unit =
    match (!lst, k) with
    | None, _ -> invalid_arg "out of bounds"
    | Some { value }, 0 -> value := v
    | Some { next }, _ -> set_value next (k - 1) v

  let rec to_list (lst : 'a mlist) =
    match !lst with
    | None -> []
    | Some { next; value } -> !value :: to_list next
end

let lst = LinkedList.empty ()

let _ =
  LinkedList.(
    insert_first lst 5;
    insert_first lst 4;
    insert_first lst 3;
    insert_first lst 2;
    insert_first lst 1)

let _ = assert (LinkedList.to_list lst = [ 1; 2; 3; 4; 5 ])
let _ = LinkedList.set_value lst 2 6
let _ = assert (LinkedList.to_list lst = [ 1; 2; 6; 4; 5 ])

(* mutable stacks *)
module type Stack = sig
  type 'a t
  (** ['a t] is the type of mutable stacks whose elements have type ['a].
      The stack is mutable not in the sense that its elements can
      be changed, but in the sense that it is not persistent:
      the operations [push] and [pop] destructively modify the stack. *)

  exception Empty
  (** Raised if [peek] or [pop] encounter the empty stack. *)

  val size : 'a t -> int
  (** size returns to size of a stack element **)

  val empty : unit -> 'a t
  (** [empty ()] is the empty stack *)

  val push : 'a -> 'a t -> unit
  (** [push x s] modifies [s] to make [x] its top element.
      The rest of the elements are unchanged. *)

  val peek : 'a t -> 'a
  (**[peek s] is the top element of [s].
     Raises: [Empty] if [s] is empty. *)

  val pop : 'a t -> unit
  (** [pop s] removes the top element of [s].
      Raises: [Empty] if [s] is empty. *)
end

module MutableRecordStack : Stack = struct
  type 'a node = { mutable next : 'a node option; value : 'a }
  type 'a t = { mutable first : 'a node option; mutable size : int }

  exception Empty

  let size s = s.size
  let empty () = { first = None; size = 0 }

  let push v s =
    s.first <- Some { next = s.first; value = v };
    s.size <- s.size + 1

  let peek s =
    match s.first with None -> raise Empty | Some { value } -> value

  let pop s =
    match s.first with
    | None -> raise Empty
    | Some { next } ->
        s.first <- next;
        s.size <- s.size - 1
end

let stack = MutableRecordStack.empty ()
let _ = assert (MutableRecordStack.size stack = 0)
let _ = MutableRecordStack.push 1 stack
let _ = MutableRecordStack.push 2 stack
let _ = assert (MutableRecordStack.peek stack = 2)
let _ = assert (MutableRecordStack.size stack = 2)
let _ = MutableRecordStack.pop stack
let _ = assert (MutableRecordStack.size stack = 1)

(* array stuff *)
let arr = [| 1; 2; 3 |]

let _ =
  assert (arr.(0) = 1);
  arr.(0) <- 4;
  assert (arr.(0) = 4)

let acc = ref 0

(* for loop over the array to sum up its values *)
let arr_sum arr =
  let len = Array.length arr in
  let acc = ref 0 in
  for x = 0 to len - 1 do
    acc := !acc + arr.(x)
  done;
  !acc

let _ = assert (arr_sum arr = 9)
