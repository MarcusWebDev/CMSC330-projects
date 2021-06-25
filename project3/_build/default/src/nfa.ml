open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let rec contains lst element = match lst with
[] -> false
| h::t -> if h = element then true else contains t element

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  fold_left (fun ac elm -> if (contains ac elm) then ac else elm::ac) [] (fold_left (fun acc lst -> lst @ acc) [] (map (fun state -> fold_left (fun acum (a,b,c) -> if a = state && s = b then c::acum else acum) [] nfa.delta) qs))

let rec build_e_closure (nfa: ('q,'s) nfa_t) lst =
  let updated_closure = union (move nfa lst None) lst in
    if eq lst updated_closure then lst else build_e_closure nfa updated_closure

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  build_e_closure nfa qs


(*For each character call move to get the states that you can move to from the current state based on that character. 
Then if there is a state you can move to on that character call move on those states with the next character. If there isn't a 
state that you can move to from those states with the current character, return false. Return true if you run out of characters and
you're in a subset of the final states.*)

let rec test_accept (nfa: ('q,char) nfa_t) chars states = match chars with 
[] -> states
| h::t -> (let new_states = e_closure nfa (move nfa states (Some h)) in 
  match new_states with
  [] -> []
  | x::y -> test_accept nfa t new_states)

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let chars = explode s in
  let final_state = test_accept nfa chars (e_closure nfa [nfa.q0]) in
  match final_state with 
  [] -> false
  | h::t -> if (intersection final_state nfa.fs) != [] then true else false


(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  fold_left (fun a x -> union [e_closure nfa (move nfa qs (Some x))] a) [] nfa.sigma

(*Go through the entire alphabet, see if there are any transitions out of the passed state to any other state. If there is, add a new transition from the passed state
  to the state that transition connects to.*)

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  fold_left (fun a x -> union [(qs, (Some x), e_closure nfa (move nfa qs (Some x)))] a) [] nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if (fold_left (fun a x -> if a = false && elem x nfa.fs then true else a) false qs) then [qs] else []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with
  [] -> dfa
  | h::t -> 
    let ns = new_states nfa h in
    let new_work = diff ns dfa.qs in
    let new_work = union new_work t in
    let new_dfa =
    {
      sigma = nfa.sigma;
      qs = union ns dfa.qs;
      q0 = dfa.q0;
      fs = union (new_finals nfa h) dfa.fs;
      delta = union (new_trans nfa h) dfa.delta 
    } in
    nfa_to_dfa_step nfa new_dfa new_work


let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let r0 = e_closure nfa [nfa.q0] in
  let dfa = {sigma = nfa.sigma; qs = [r0]; q0 = r0; fs = []; delta = []} in 
  nfa_to_dfa_step nfa dfa [r0]

