(****************************)
(* Part 0: Helper Functions *)
(****************************)

let rec check_primality x y =
  if x = 2 then
    true
  else if x < 2 then
    false
  else if x <= y then
    true
  else if (x mod y = 0) then
    false
  else
    check_primality x (y+1)

let rec length lst =
  match lst with 
  | [] -> 0
  | (h::t) -> 1 + length t 

let rec check_palindrome list1 list2 = 
  match list1 with
  | [] -> true
  | (h::t) -> match list2 with
    | [] -> true
    | (m::n) -> if h = m then
        check_palindrome t n
    else
      false


(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup =
  match tup with
  | (x, y, z) ->  (z, y, x)

let abs x =
  if x >= 0 then
    x
  else
    x * (-1) 

let area x y = 
  match x with 
    (x1, x2) -> match y with
      (y1, y2) -> abs((y1 - x1) * (y2 - x2))

let volume x y = 
  match x with 
    (x1, x2, x3) -> match y with
      (y1, y2, y3) -> abs((y1 - x1) * (y2 - x2) * (y3 - x3))

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
  if n = 0 then
    0
  else if n <= 2 then
    1
  else
    (fibonacci (n - 1)) + (fibonacci (n - 2))

let rec pow x y = 
  if y = 1 then
    x
  else if y = 0 then
    1
  else 
    x * pow x (y - 1)

let rec log x y = 
  if y < x then
    0
  else
    1 + log x (y / x)

let rec is_prime x = 
  check_primality x 2

let rec next_prime x = 
  if x <= 2 then
    2
  else 
    let found = check_primality x 2 in
      if found = true then
        x
      else
        next_prime (x + 1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
  if idx = 0 then
    match lst with
    | [] -> failwith "Out of bounds"
    | (h::_) -> h
  else
    match lst with
    | [] -> failwith "Out of bounds"
    | (h::t) -> get (idx - 1) t

let rec larger list1 list2 = 
  if length list1 > length list2 then
    list1
  else if length list1 < length list2 then
    list2
  else
    []

let rec combine lst1 lst2 = 
  match lst1 with
  | [] -> lst2
  | (h::t) -> h::(combine t lst2)

let rec reverse lst = 
  match lst with 
  | [] -> []
  | (h::t) -> combine (reverse t) (h::[])

let rec rotate shift lst = 
  if shift = 0 then
    lst
  else
    match lst with
    | [] -> lst
    | (h::t) -> rotate (shift - 1) (t @ [h])

let rec is_palindrome lst = 
  check_palindrome lst (reverse lst)
  