open Contextfree
open Types
open Grammar
open Exercises

let to_string = string_of_sentform

let%test "example_test" = derive todo [ 0; 1; 0; 1; 2 ] |> to_string = "01011010"

(** Following the example test, complete the tests below.

    For each test, provide the right sequence of productions
    to generate the word on the right of the "=".
*)

(* #### Exercise 1 *)
let%test "zero_n_one_n_1" = derive zero_n_one_n [1] |> to_string = ""
let%test "zero_n_one_n_2" = derive zero_n_one_n [0 ; 1] |> to_string = "01"
let%test "zero_n_one_n_3" = derive zero_n_one_n [0;0;0;0;0;0;0;0;0;0;1] |> to_string = "00000000001111111111"


let%test "palindromes_1" = derive palindromes [1;1;2] |> to_string = "11011"
let%test "palindromes_2" = derive palindromes [0;1;4] |> to_string = "0110"
let%test "palindromes_3" = derive palindromes [0;1;1;1;2] |> to_string = "011101110"


let%test "balanced_parentheses_1" = derive balanced_parentheses [4;1;0;4;2;0;3;0] |> to_string = "()[]{}"
let%test "balanced_parentheses_2" = derive balanced_parentheses [4;1;3;0;2;0] |> to_string = "({})[]"
let%test "balanced_parentheses_3" = derive balanced_parentheses [1;3;4;2;0;2;4;3;0;1;0] |> to_string = "({[][{}()]})"


let%test "zero_one_same_1" = derive same_amount [0] |> to_string = ""
let%test "zero_one_same_2" = derive same_amount [2; 0; 1; 0; 0] |> to_string = "1001"
let%test "zero_one_same_3" = derive same_amount [1;1;0;0;1;0;1;0;0] |> to_string = "00110101"
let%test "zero_one_same_4" = derive same_amount [2;0;1;1;0;0;2;0;0] |> to_string = "10001110"
