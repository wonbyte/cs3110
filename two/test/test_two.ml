open OUnit2
open Two

let tests =
  "test suite for chapter two"
  >::: [
         ( "int values" >:: fun _ ->
           assert_equal 42 (7 * (1 + 2 + 3)) ~printer:string_of_int );
         ( "string values" >:: fun _ ->
           assert_equal "CS 3110" ("CS " ^ string_of_int 3110) );
         ("int operators" >:: fun _ -> assert_equal 420 (42 * 10));
         ("float operators" >:: fun _ -> assert_equal 1.57 (3.14 /. 2.0));
         ( "float powers" >:: fun _ ->
           assert_equal 23053.9333248000075 (4.2 ** 7.) );
         ("assert string one" >:: fun _ -> assert_equal true ("hi" = "hi"));
         ("not equal one" >:: fun _ -> assert_equal true (not (2110 = 3110)));
         ("not equal two" >:: fun _ -> assert_equal true (2110 <> 3110));
         ("fun double" >:: fun _ -> assert_equal 4 (double 2));
         ("fun double two" >:: fun _ -> assert_equal 14 (double 7));
         ("cube" >:: fun _ -> assert_equal 0. (cube 0.));
         ("cube two" >:: fun _ -> assert_equal 1. (cube 1.));
         ("cube three" >:: fun _ -> assert_equal 27. (cube 3.));
         ("sign one" >:: fun _ -> assert_equal 0 (sign 0));
         ("sign two" >:: fun _ -> assert_equal 1 (sign 10));
         ("sign three" >:: fun _ -> assert_equal (-1) (sign (-10)));
         ("infix one" >:: fun _ -> assert_equal 1.5 (1.0 +/. 2.0));
         ("infix two" >:: fun _ -> assert_equal 0. (0. +/. 0.));
       ]

let _ = run_test_tt_main tests
