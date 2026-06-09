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
