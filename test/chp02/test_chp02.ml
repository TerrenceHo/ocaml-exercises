open OUnit2
open Chp02.Exercises

let test_operator _ =
  (* the custom ( $ ) operator: x*y + x *)
  assert_equal 10 (2 $ 3)

let suite = "ch02" >::: [ "operator" >:: test_operator ]
let () = run_test_tt_main suite
