(* [add1 lst] adds 1 to each element of [lst] *)
let rec add1 = function
  | [] -> []
  | h :: t ->
      let f x = x + 1 in
      f h :: add1 t

let lst1 = add1 [ 1; 2; 3 ]

(* concat_bang lst] concatenates "!" to each element of [lst] *)
let rec concat_bang = function
  | [] -> []
  | h :: t ->
      let f x = x ^ "!" in
      f h :: concat_bang t

let lst2 = concat_bang [ "sweet"; "salty" ]
let rec add1' f = function [] -> [] | h :: t -> f h :: add1' f t

(* [add1 lst] adds 1 o each element of [lst] *)
let add1 = add1' (fun x -> x + 1)
let rec concat_bang' f = function [] -> [] | h :: t -> f h :: concat_bang' f t

(* concat_bang lst] concatenates "!" to each element of [lst] *)
let concat_bang = concat_bang' (fun x -> x ^ "!")
let rec transform f = function [] -> [] | h :: t -> f h :: transform f t

(* [add1 lst] adds 1 to each element of [lst] *)
let add1 = transform (fun x -> x + 1)

(* [concat_bang lst] concatenates "!" to each element of [lst] *)
let concat_bang = transform (fun x -> x ^ "!")
let rec map f = function [] -> [] | h :: t -> f h :: map f t

(** [add1 lst] adds 1 to each element of [lst] *)
let add1 = map (fun x -> x + 1)

(** [concat_bang lst] concatenates "!" to each element of [lst] *)
let concat_bang = map (fun x -> x ^ "!")

let rec map f = function
  | [] -> []
  | h :: t ->
      let h' = f h in
      h' :: map f t

let p x =
  print_int x;
  print_newline ();
  x + 1

let lst2 = map p [ 1; 2 ]

let rec rev_map_aux f acc = function
  | [] -> acc
  | h :: t -> rev_map_aux f (f h :: acc) t

let rev_map f = rev_map_aux f []
let lst = rev_map (fun x -> x + 1) [ 1; 2; 3 ]
let lst' = List.rev (List.rev_map (fun x -> x + 1) [ 1; 2; 3 ])

(* [even n] is whether [n] is even. *)
let even n = n mod 2 = 0

(* [evens lst] is the sublist of [lst] containing only even numbers . *)
let rec evens = function
  | [] -> []
  | h :: t -> if even h then h :: evens t else evens t

let lst1 = evens [ 1; 2; 3; 4 ]

(* [odd n] is whether [n] is odd. *)
let odd n = n mod 2 <> 0

(* [odds lst] is the sublist of [lst] containing only odd numbers. *)
let rec odds = function
  | [] -> []
  | h :: t -> if odd h then h :: odds t else odds t

let lst2 = odds [ 1; 2; 3; 4 ]

let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let evens = filter even
let odds = filter odd

let rec filter_aux p acc = function
  | [] -> acc
  | h :: t -> if p h then filter_aux p (h :: acc) t else filter_aux p acc t

let filter p = filter_aux p []
let lst = filter even [ 1; 2; 3; 4 ]
