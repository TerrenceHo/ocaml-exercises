open OUnit2

let rec sum l = match l with [] -> 0 | h :: t -> h + sum t;;

print_int (sum [ 1; 2; 3; 4; 5; 6; 7 ]);;
print_endline ""

let rec sum_tailcall_helper l r =
  match l with [] -> r | h :: t -> sum_tailcall_helper t (r + h)

let sum_tailcall l = (sum_tailcall_helper [@tailcall]) l 0

let tests =
  "test suite for sum"
  >::: [
         ("empty" >:: fun _ -> assert_equal 0 (sum []));
         ("singleton" >:: fun _ -> assert_equal 1 (sum [ 1 ]));
         ("two_elements" >:: fun _ -> assert_equal 3 (sum [ 1; 2 ]));
         ( "longer_list" >:: fun _ ->
           assert_equal 28 (sum [ 1; 2; 3; 4; 5; 6; 7 ]) );
         ("tailcall_empty" >:: fun _ -> assert_equal 0 (sum_tailcall []));
         ("tailcall_singleton" >:: fun _ -> assert_equal 1 (sum_tailcall [ 1 ]));
         ( "tailcall_two_elements" >:: fun _ ->
           assert_equal 3 (sum_tailcall [ 1; 2 ]) );
         ( "tailcall_longer_list" >:: fun _ ->
           assert_equal 28 (sum_tailcall [ 1; 2; 3; 4; 5; 6; 7 ]) );
       ]

let _ = run_test_tt_main tests
