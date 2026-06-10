open OUnit2
open Chp03

let make_day_test name expected input =
  name >:: fun _ ->
  assert_equal expected (int_of_day input) ~printer:string_of_int

let day_tests =
  "test suit for int_of_day"
  >::: [
         make_day_test "Sun" 1 Sun;
         make_day_test "Mon" 2 Mon;
         make_day_test "Tue" 3 Tue;
         make_day_test "Wed" 4 Wed;
         make_day_test "Thu" 5 Thu;
         make_day_test "Fri" 6 Fri;
         make_day_test "Sat" 7 Sat;
       ]

let sum_tests =
  "test suite for sum"
  >::: [
         ("empty" >:: fun _ -> assert_equal 0 (sum []));
         ("singleton" >:: fun _ -> assert_equal 1 (sum [ 1 ]));
         ("two_elements" >:: fun _ -> assert_equal 3 (sum [ 1; 2 ]));
       ]

(* Print expected outputs, without repetition *)
let make_sum_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (sum input) ~printer:string_of_int

let sum_tests_2 =
  "test suite for sum"
  >::: [
         make_sum_test "empty" 0 [];
         make_sum_test "singleton" 1 [ 1 ];
         make_sum_test "two_elements" 3 [ 1; 2 ];
       ]

let _ = run_test_tt_main sum_tests
let _ = run_test_tt_main sum_tests_2
let _ = run_test_tt_main day_tests

let make_product_test name expected input =
  name >:: fun _ -> assert_equal expected (product input) ~printer:string_of_int

let product_tests =
  "test suit for product"
  >::: [
         make_product_test "empty" 1 [];
         make_product_test "singletone" 1 [ 1 ];
         make_product_test "single 2" 2 [ 2 ];
         make_product_test "six" 6 [ 1; 2; 3 ];
         make_product_test "multiple" 100 [ 10; 2; 5 ];
       ]

let _ = run_test_tt_main product_tests

let make_fifth_ele_test name expected input =
  name >:: fun _ ->
  assert_equal expected (Chp03.fifth_ele input) ~printer:string_of_int

let fifth_ele_tests =
  "test suite for fifth_ele"
  >::: [
         make_fifth_ele_test "empty" 0 [];
         make_fifth_ele_test "less than 5" 0 [ 1; 2 ];
         make_fifth_ele_test "equal" 5 [ 1; 2; 3; 4; 5 ];
         make_fifth_ele_test "more than 5" 5 [ 1; 2; 3; 4; 5; 6; 7 ];
       ]

let _ = run_test_tt_main fifth_ele_tests
let long_list = Chp03.from 0 10000000
let _ = assert_equal [ 0; 1; 2; 3; 4 ] (take_tr 5 long_list)
