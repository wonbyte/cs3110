open OUnit2
open Tdd

let make_next_weekday_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (next_weekday input)

let tests =
  "test suite for next_weekday"
  >::: [
         make_next_weekday_test "tue_after_mon" Tuesday Monday;
         make_next_weekday_test "wed_after_tue" Wednesday Tuesday;
         make_next_weekday_test "thu_after_wed" Thursday Wednesday;
         make_next_weekday_test "fri_after_thu" Friday Thursday;
       ]

let _ = run_test_tt_main tests
