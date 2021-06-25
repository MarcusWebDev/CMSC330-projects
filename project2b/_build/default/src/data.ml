open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let intoption_to_int x = 
  match x with
  None -> raise (Failure "Cannot convert None to Int")
  | Some i -> i

let rec int_insert x t =
  match t with 
  IntLeaf -> IntNode (x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode (i1, i2, t1, t2, t3) -> 
      if i2 = None then
        if x = i1 then 
          t
        else if x < i1 then
          IntNode (x, Some i1, t1, t2, t3)
        else
          IntNode (i1, Some x, t1, t2, t3)
      else if x < i1 then
        IntNode (i1, i2, int_insert x t1, t2, t3)
      else if x > i1 && x < intoption_to_int i2 then
        IntNode (i1, i2, t1, int_insert x t2, t3)
      else if x = i1 || x = intoption_to_int i2 then
        t
      else 
        IntNode (i1, i2, t1, t2, int_insert x t3) 


let rec int_mem x t =
  match t with
  IntLeaf -> false
  | IntNode (i1, i2, t1, t2, t3) -> 
    if i2 = None then
      if x = i1 then
        true
      else
        false
    else if x = i1 || x = intoption_to_int i2 then
      true
    else if x < i1 then
      int_mem x t1
    else if x >= i1 && x <= intoption_to_int i2 then
      int_mem x t2
    else
      int_mem x t3

let rec int_size t =
  match t with
  IntLeaf -> 0
  | IntNode (i1, i2, t1, t2, t3) -> 
    if i2 = None then
      1
    else
      2 + (int_size t1) + (int_size t2) + (int_size t3)

let rec int_max t =
  match t with
  IntLeaf -> raise (Invalid_argument "int_max")
  | IntNode (i1, i2, t1, t2, t3) -> 
    if i2 = None then
      i1
    else
      if t3 = IntLeaf then
        intoption_to_int i2
      else
        int_max t3


(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let kvoption_to_k kv = 
  match kv with
  None -> raise (Failure "Cannot convert None to Key")
  | Some (k, v) -> k

  let kvoption_to_v kv = 
    match kv with
    None -> raise (Failure "Cannot convert None to Key")
    | Some (k, v) -> v

let rec map_put k v t = 
  match t with 
  MapLeaf -> MapNode ((k,v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode ((k1,v1), someKV, t1, t2, t3) -> 
      if someKV = None then
        if k = k1 then
          raise (Invalid_argument "map_put")
        else if k < k1 then
          MapNode ((k, v), Some (k1, v1), t1, t2, t3)
        else
          MapNode ((k1, v1), Some (k, v), t1, t2, t3)
      else if k < k1 then
        MapNode ((k1, v1), someKV, map_put k v t1, t2, t3)
      else if k > k1 && k < kvoption_to_k someKV then
        MapNode ((k1, v1), someKV, t1, map_put k v t2, t3)
      else if k = k1 || k = kvoption_to_k someKV then
        raise (Invalid_argument "map_put")
      else 
        MapNode ((k1, v1), someKV, t1, t2, map_put k v t3) 

let rec map_contains k t = 
  match t with
  MapLeaf -> false
  | MapNode ((k1, v1), someKV, t1, t2, t3) -> 
    if someKV = None then
      if k = k1 then
        true
      else
        false
    else if k = k1 || k = kvoption_to_k someKV then
      true
    else if k < k1 then
      map_contains k t1
    else if k > k1 && k < kvoption_to_k someKV then
      map_contains k t2
    else
      map_contains k t3

let rec map_get k t =
  match t with
  MapLeaf -> raise (Invalid_argument "map_get")
  | MapNode ((k1, v1), someKV, t1, t2, t3) -> 
    if someKV = None then
      if k = k1 then
        v1
      else
        raise (Invalid_argument "map_get")
    else if k = k1 then
      v1
    else if k = (kvoption_to_k someKV) then
      kvoption_to_v someKV
    else if k < k1 then
      map_get k t1
    else if k > k1 && k < kvoption_to_k someKV then
      map_get k t2
    else
      map_get k t3

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

type lookup_table = (string * int) list list

let empty_table = []

let push_scope (table:lookup_table) : lookup_table = 
  match table with
  [] -> [[]]
  | (h::t) -> []::(h::t)

let pop_scope (table:lookup_table) : lookup_table =
  match table with
  [] -> failwith "No scopes remain!"
  | (h::t) -> t

let add_var name value (table:lookup_table) : lookup_table =
  match table with
  [] -> failwith "There are no scopes to add a variable to!"
  | (h::t) -> let total = fold (fun a x -> if x = 1 then a + 1 else a) 0 (map (fun (curName, curVal) -> if curName = name then 1 else 0) h) in
    if total > 0 then
      failwith "Duplicate variable binding in scope!"
    else
      ([(name, value)]@h)::t

let rec lookup_helper name lst =
  match lst with
  [] -> (false, 0)
  | (h, v)::t -> if h = name then (true, v) else lookup_helper name t

let rec lookup name (table:lookup_table) = 
  match table with 
  [] -> failwith "Variable not found!"
  | (h::t) -> match lookup_helper name h with
    (b, v) -> if b then v else lookup name t