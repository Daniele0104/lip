open Types

(* Use this grammar record as a blueprint for the exercises. *)
let todo : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S0"; (* 0 *)
        S --> "1S1"; (* 1 *)
        S --> "";    (* 2 *)
      ];
    start = S;
  }


(* #### Exercise 1, easy (zero_n_one_n) *)
let zero_n_one_n : grammar =
  {
    symbols = [S];
    terminals = ['0';'1'];
    productions = [
      S --> "0S1";
      S --> "";
    ];
    start = S;
  }


(* #### Exercise 2, easy (palindromes) *)
let palindromes : grammar =
  {
    symbols = [S];
    terminals = ['0';'1'];
    productions = [
      S --> "0S0";
      S --> "1S1"; 
      S --> "0";
      S --> "1";
      S --> "";
    ];
    start = S;
  }


(* #### Exercise 3, medium (balanced_parentheses) *)
let balanced_parentheses : grammar = 
  {
    symbols = [S];
    terminals = ['('; ')'; '['; ']'; '{'; '}'];
    productions = [
      S --> "";
      S --> "(S)";
      S --> "[S]";
      S --> "{S}";
      S --> "SS";
    ];
    start = S;
  }


(* #### Exercise 4, hard (same_amount)

   Hint: model the language of words where the number of 0's is
   one greater than the number of 1's and viceversa, then combine them.
*)
let same_amount : grammar = 
  {
    symbols = [S;A];
    terminals = ['0';'1'];
    productions = [
      S --> "";
      S --> "0S1S";
      S --> "1S0S";
    ];
    start = S;
  }
