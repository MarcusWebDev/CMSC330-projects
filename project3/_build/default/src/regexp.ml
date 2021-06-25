open List
open Nfa
open Sets

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)
let rec nfa_builder regexp first nfa_previous_state = match regexp with
  Empty_String -> nfa_previous_state
  | Char x -> (
    let new_state = fresh() in
    match nfa_previous_state with
    (nfa, previous_state) ->
    ({
      sigma = union nfa.sigma [x];
      qs = union nfa.qs [new_state];
      q0 = nfa.q0;
      fs = nfa.fs;
      delta = union nfa.delta [(previous_state, Some x, new_state)]
    }, new_state))
  | Union (a, b) -> (
    let new_left_state = fresh() in
    let new_right_state = fresh() in
    match nfa_previous_state with
    (nfa, previous_state) ->
      let new_left_nfa = ({
        sigma = nfa.sigma;
        qs = union nfa.qs (new_left_state::new_right_state::[]);
        q0 = nfa.q0;
        fs = nfa.fs;
        delta = union nfa.delta ((previous_state, None, new_left_state)::(previous_state, None, new_right_state)::[])
      }, new_left_state) in
      let new_right_nfa = ({
        sigma = nfa.sigma;
        qs = union nfa.qs (new_left_state::new_right_state::[]);
        q0 = nfa.q0;
        fs = nfa.fs;
        delta = union nfa.delta ((previous_state, None, new_left_state)::(previous_state, None, new_right_state)::[])
      }, new_right_state) in
      let left_nfa = nfa_builder a false new_left_nfa in
      let right_nfa = nfa_builder b false new_right_nfa in 
      let new_intersection_state = fresh() in
      match left_nfa with 
      (n1, ls) -> match right_nfa with
        (n2, rs) -> 
          ({
            sigma = union n1.sigma n2.sigma;
            qs = new_intersection_state::(union n1.qs n2.qs);
            q0 = nfa.q0;
            fs = if first then [new_intersection_state] else nfa.fs;
            delta = (ls, None, new_intersection_state)::(rs, None, new_intersection_state)::(union n1.delta n2.delta)
          }, new_intersection_state))
  | Concat (c, d) -> (
    match nfa_previous_state with
    (nfa, previous_state) ->
      let left_nfa = nfa_builder c false (nfa, previous_state) in
      match left_nfa with
      (l, ls) -> 
        let right_enter = fresh() in
        let right_nfa = nfa_builder d false (nfa, right_enter) in
        match right_nfa with
        (r, rs) ->
          ({
            sigma = union l.sigma r.sigma;
            qs = union l.qs r.qs;
            q0 = nfa.q0;
            fs = if first then [rs] else nfa.fs;
            delta = (ls, None, right_enter)::(union l.delta r.delta)
          }, rs))
  | Star y -> (
    let enter_state = fresh() in
    let exit_state = fresh() in
    match nfa_previous_state with 
    (nfa, previous_state) -> (
      let repeatable_nfa = nfa_builder y false (nfa, enter_state) in
      match repeatable_nfa with
      (r, rs) ->
        ({
          sigma = union r.sigma nfa.sigma;
          qs = union r.qs nfa.qs;
          q0 = nfa.q0;
          fs = if first then [previous_state; exit_state] else nfa.fs;
          delta = (previous_state, None, enter_state)::(rs, None, exit_state)::(exit_state, None, previous_state)::(previous_state, None, exit_state)::(union nfa.delta r.delta)
        }, exit_state)))


let regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
  let starting_state = fresh() in
  let base_nfa_state = ({
    sigma = [];
    qs = [starting_state];
    q0 = starting_state;
    fs = [];
    delta = []
  }, starting_state) in
  match regexp with
  Empty_String -> 
    {
      sigma = [];
      qs = [starting_state];
      q0 = starting_state;
      fs = [];
      delta = []
    }
  | Char x -> 
    let ending_state = fresh() in 
      {
        sigma = [x];
        qs = [starting_state; ending_state];
        q0 = starting_state;
        fs = [ending_state];
        delta = [(starting_state, Some x, ending_state)]
      }
  | Union (a, b) -> (let final_nfa = nfa_builder regexp true base_nfa_state in
    match final_nfa with
    (nfa, final_state) -> nfa)
  | Concat (c, d) -> (let final_nfa = nfa_builder regexp true base_nfa_state in
    match final_nfa with
    (nfa, final_state) -> nfa)
  | Star y -> (let final_nfa = nfa_builder regexp true base_nfa_state in
    match final_nfa with
    (nfa, final_state) -> nfa)
  

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
