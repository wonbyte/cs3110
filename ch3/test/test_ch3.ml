open OUnit2
open Ch3

let tests =
  "test suite for chapter three"
  >::: [
         ( "product" >:: fun _ ->
           assert_equal 6 (product [ 1; 2; 3 ]) ~printer:string_of_int );
         ("concat" >:: fun _ -> assert_equal "hi" (concat [ "h"; "i" ]));
       ]

let _ = run_test_tt_main tests
