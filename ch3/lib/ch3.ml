(********************************************************************
 * exercise: list expressions
 ********************************************************************)
let lst1 = [ 1; 2; 3; 4; 5 ]
let lst2 = [ 1; 2; 3; 4; 5 ]
let lst3 = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(********************************************************************
 * exercise: product
 ********************************************************************)

(* returns the product of all the elements of [lst], or [1] if [lst] is empty. *)
let rec product = function [] -> 1 | h :: t -> h * product t

(********************************************************************
 * exercise: concat
 ********************************************************************)

(* returns: the concatenation of all the strings in the list, of the empty string
   if list is empty *)
let rec concat = function [] -> "" | h :: t -> h ^ concat t

(********************************************************************
 * exercise: patterns
 ********************************************************************)

(* returns: whether the first element of the input is ["bigred"] *)
let first_is_bigred = function [] -> false | h :: _ -> h = "bigred"

(* returns: whether the input has exactly two or four elements *)
let two_or_four = function
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

(* returns whether the first two elements of the input are equal *)
let eq_first_two = function a :: b :: _ -> a = b | _ -> false

(********************************************************************
 * exercise: library
 ********************************************************************)

(* returns: the fifth element of the input list, or zero if the list is empty *)
let fifth_element lst = if List.length lst >= 5 then List.nth lst 4 else 0

(* returns: the input list, sorted in descending order *)
let sort_list_descending lst = lst |> List.sort Stdlib.compare |> List.rev

(* returns: the last element of [lst]
   requires: [lst] is nonempty *)
let last_element lst = lst |> List.rev |> List.hd

(* returns: whether [lst] contains any zeros *)
let any_zeros lst = List.exists (fun x -> x = 0) lst
