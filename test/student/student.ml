open OUnit2
open Basics

open OUnit2
open Basics

let test_rev_tup _ =
  assert_equal ("x", 42) (rev_tup (42, "x")) ~msg:"rev_tup (s1)";
  assert_equal (3.14, true) (rev_tup (true, 3.14)) ~msg:"rev_tup (s2)";
  assert_equal ('b', 'a') (rev_tup ('a', 'b')) ~msg:"rev_tup (s3)";
  assert_equal ("ok", "ok") (rev_tup ("ok", "ok")) ~msg:"rev_tup (s4)"

let test_rev_triple _ =
  assert_equal (false, 0, "z") (rev_triple ("z", 0, false)) ~msg:"rev_triple (s1)";
  assert_equal (3.0, 2, 1) (rev_triple (1, 2, 3.0)) ~msg:"rev_triple (s2)";
  assert_equal (0, 0, 0) (rev_triple (0, 0, 0)) ~msg:"rev_triple (s3)";
  assert_equal ("c", "b", "a") (rev_triple ("a", "b", "c")) ~msg:"rev_triple (s4)"

let test_is_odd _ =
  assert_equal true (is_odd (-7)) ~msg:"is_odd (s1)";
  assert_equal false (is_odd (-8)) ~msg:"is_odd (s2)";
  assert_equal true (is_odd 13) ~msg:"is_odd (s3)";
  assert_equal false (is_odd 100) ~msg:"is_odd (s4)"

let test_is_older _ =
  assert_equal true (is_older (1999, 12, 31) (2000, 1, 1)) ~msg:"is_older (s1)";
  assert_equal true (is_older (2025, 5, 10) (2025, 6, 1)) ~msg:"is_older (s2)";
  assert_equal true (is_older (1990, 4, 14) (1990, 4, 15)) ~msg:"is_older (s3)";
  assert_equal false (is_older (2024, 3, 3) (2024, 3, 3)) ~msg:"is_older (s4)"

let test_to_us_format _ =
  assert_equal (7, 4, 1776) (to_us_format (1776, 7, 4)) ~msg:"to_us_format (s1)";
  assert_equal (12, 31, 2000) (to_us_format (2000, 12, 31)) ~msg:"to_us_format (s2)";
  assert_equal (1, 1, 1990) (to_us_format (1990, 1, 1)) ~msg:"to_us_format (s3)";
  assert_equal (9, 26, 2025) (to_us_format (2025, 9, 26)) ~msg:"to_us_format (s4)"

let test_pow _ =
  assert_equal 1 (pow 7 0) ~msg:"pow (s1)";
  assert_equal 1024 (pow 2 10) ~msg:"pow (s2)";
  assert_equal 16 (pow (-2) 4) ~msg:"pow (s3)";
  assert_equal (-32) (pow (-2) 5) ~msg:"pow (s4)";
  assert_equal 0 (pow 0 5) ~msg:"pow (s5)"

let test_fac _ =
  assert_equal 1 (fac 1) ~msg:"fac (s1)";
  assert_equal 6 (fac 3) ~msg:"fac (s2)";
  assert_equal 720 (fac 6) ~msg:"fac (s3)";
  assert_equal 40320 (fac 8) ~msg:"fac (s4)"

let test_get_nth _ =
  assert_equal 'q' (get_nth (0, ['q'])) ~msg:"get_nth (s1)";
  assert_equal "eel" (get_nth (2, ["cat"; "dog"; "eel"])) ~msg:"get_nth (s2)";
  assert_equal 9 (get_nth (3, [6; 7; 8; 9; 10])) ~msg:"get_nth (s3)"

let test_larger _ =
  assert_equal [] (larger [1; 2] [3; 4]) ~msg:"larger (s1)"; 
  assert_equal ["x"; "y"; "z"] (larger [] ["x"; "y"; "z"]) ~msg:"larger (s2)";
  assert_equal [0; 0; 0; 0] (larger [0; 0; 0; 0] [1]) ~msg:"larger (s3)";
  assert_equal ["a"] (larger ["a"] []) ~msg:"larger (s4)"

let test_sum _ =
  assert_equal 0 (sum [] [0]) ~msg:"sum (s1)";
  assert_equal (-6) (sum [-1; -2] [-3; 0]) ~msg:"sum (s2)";
  assert_equal 100 (sum [10; 20; 30] [40]) ~msg:"sum (s3)";
  assert_equal 15 (sum [5; -5; 10] [0; 5; 0]) ~msg:"sum (s4)"

let suite =
  "student" >::: [
    "rev_tup" >:: test_rev_tup;
    "rev_triple" >:: test_rev_triple;
    "is_odd" >:: test_is_odd;
    "is_older" >:: test_is_older;
    "to_us_format" >:: test_to_us_format;
    "pow" >:: test_pow;
    "fac" >:: test_fac;
    "get_nth" >:: test_get_nth;
    "larger" >:: test_larger;
    "sum" >:: test_sum;
  ]

let _ = run_test_tt_main suite