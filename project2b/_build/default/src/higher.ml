open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let is_present lst x = map (fun m -> if m = x then 1 else 0) lst

let count_occ lst target = fold (fun a x -> x + a) 0 (is_present lst target)

let contains_elem lst e = 
  let sum = count_occ lst e in 
  if sum > 0 then 
    true 
  else 
    false

let uniq lst = fold (fun a x -> if not (contains_elem a x) then x::a else a) [] lst

let assoc_list lst = 
  let uniqueList = uniq lst in 
  fold (fun a x -> (x, count_occ lst x)::a) [] uniqueList

let ap fns args = fold (fun a x -> a @ (map x args)) [] fns
