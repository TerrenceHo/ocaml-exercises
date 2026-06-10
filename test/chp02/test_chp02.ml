open OUnit2
open Chp02

let double_tests =
  "test suite for double"
  >::: [
         ("zero" >:: fun _ -> assert_equal 0 (double 0));
         ("double 1" >:: fun _ -> assert_equal 2 (double 1));
         ("double 2" >:: fun _ -> assert_equal 4 (double 2));
         ("double 8" >:: fun _ -> assert_equal 16 (double 8));
       ]

let _ = run_test_tt_main double_tests
