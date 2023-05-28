type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec size = function Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r
let tree1 = Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf));;

print_int (size tree1);;
print_endline ""

(* A tree with records as nodes *)
type 'a treeNode = Leaf | Node of 'a node
and 'a node = { value : 'a; left : 'a treeNode; right : 'a treeNode }

let rec size = function
  | Leaf -> 0
  | Node { value; left; right } -> 1 + size left + size right

let preorder tree =
  let rec preorder_acc acc tree =
    match tree with
    | Leaf -> acc
    | Node { value; left; right } ->
        value :: preorder_acc (preorder_acc acc right) left
  in
  preorder_acc [] tree

let inorder tree =
  let rec inorder_acc acc tree =
    match tree with
    | Leaf -> acc
    | Node { value; left; right } ->
        let inorder_acc_right = inorder_acc acc right in
        let inorder_acc_value = value :: inorder_acc_right in
        inorder_acc inorder_acc_value left
  in
  inorder_acc [] tree

let postorder tree =
  let rec postorder_acc acc tree =
    match tree with
    | Leaf -> acc
    | Node { value; left; right } ->
        let postorder_acc_value = value :: acc in
        let postorder_acc_right = postorder_acc postorder_acc_value right in
        postorder_acc postorder_acc_right left
  in
  postorder_acc [] tree

(* 4 *)
(* 2       6 *)
(* 1      3  5     7 *)
let tree2 =
  Node
    {
      value = 4;
      left =
        Node
          {
            value = 2;
            left = Node { value = 1; left = Leaf; right = Leaf };
            right = Node { value = 3; left = Leaf; right = Leaf };
          };
      right =
        Node
          {
            value = 6;
            left = Node { value = 5; left = Leaf; right = Leaf };
            right = Node { value = 7; left = Leaf; right = Leaf };
          };
    }

let print_list l =
  let rec print_list_helper start = function
    | [] -> ()
    | h :: t ->
        if not start then print_string ", " else ();
        print_int h;
        print_list_helper false t
  in
  print_string "[";
  print_list_helper true l;
  print_string "]"
;;

print_int (size tree2);;
print_endline "";;
print_list (preorder tree2);;
print_endline "";;
print_list (inorder tree2);;
print_endline "";;
print_list (postorder tree2);;
print_endline ""
