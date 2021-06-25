open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

let retrieve_int value = 
  match value with
  | Int i -> i
  | _ -> raise (TypeError "Int Expected")

let retrieve_bool value = 
  match value with
  | Bool b -> b
  | _ -> raise (TypeError "Int Expected")

let retrieve_string value = 
  match value with
  | String s -> s
  | _ -> raise (TypeError "Int Expected")

let eval_arithmetic_operator op val1 val2 =
  let v1, v2 = (match val1, val2 with
    | Int i, Int j -> (i, j)
    | _ -> raise (TypeError "Expected Int values")) in
  match op with
  | Add -> Int (v1 + v2)
  | Sub -> Int (v1 - v2)
  | Mult -> Int (v1 * v2)
  | Div -> if v2 = 0 then raise (DivByZeroError) else Int (v1 / v2)
  | _ -> raise (TypeError "Expected arithmetic operator")

let eval_comparison_operator op val1 val2 =
  let v1, v2 = (match val1, val2 with
    | Int i, Int j -> (i, j)
    | _ -> raise (TypeError "Expected Int values")) in
  match op with
  | Greater -> Bool (v1 > v2)
  | GreaterEqual -> Bool (v1 >= v2)
  | Less -> Bool (v1 < v2)
  | LessEqual -> Bool (v1 <= v2)
  | _ -> raise (TypeError "Expected comparison operator")

let eval_concat_operator op val1 val2 = 
  let v1, v2 = (match val1, val2 with
    | String s1, String s2 -> (s1, s2)
    | _ -> raise (TypeError "Expected String values")) in
  match op with
  Concat -> String (v1 ^ v2)
  | _ -> raise (TypeError "Expected concat operator")

let eval_equality_operator op val1 val2 =
  let value = (match val1, val2 with
    | Int i, Int j -> i = j
    | String s1, String s2 -> s1 = s2
    | Bool b1, Bool b2 -> b1 = b2
    | _ -> raise (TypeError "Expected two Int, String, or Bool values")) in
  match op with
  Equal -> Bool (value)
  | NotEqual -> Bool (not value)
  | _ -> raise (TypeError "Expected equality operator")

let eval_junction_operator op val1 val2 =
  let v1, v2 = (match val1, val2 with
    | Bool b1, Bool b2 -> (b1, b2)
    | _ -> raise (TypeError "Expected Bool values")) in
  match op with
  | Or -> Bool (v1 || v2)
  | And -> Bool (v1 && v2)
  | _ -> raise (TypeError "Expected junction operator")



(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
    | Value v -> v
    | ID v -> lookup env v
    | Fun (id, ex) -> Closure (env, id, ex) 
    | Not ex -> 
      let evaluated = (eval_expr env ex) in 
      (
        match evaluated with
        Bool b -> Bool (not b)
        | _ -> raise (TypeError "Expected type bool")
      )
    | Binop (op, ex1, ex2) ->
      let eval1 = eval_expr env ex1 in 
      let eval2 = eval_expr env ex2 in
      (
        match op with
        | Add -> eval_arithmetic_operator Add eval1 eval2
        | Sub -> eval_arithmetic_operator Sub eval1 eval2
        | Mult -> eval_arithmetic_operator Mult eval1 eval2
        | Div -> eval_arithmetic_operator Div eval1 eval2

        | Greater -> eval_comparison_operator Greater eval1 eval2
        | GreaterEqual -> eval_comparison_operator GreaterEqual eval1 eval2
        | Less -> eval_comparison_operator Less eval1 eval2
        | LessEqual -> eval_comparison_operator LessEqual eval1 eval2

        | Concat -> eval_concat_operator Concat eval1 eval2

        | Equal -> eval_equality_operator Equal eval1 eval2
        | NotEqual -> eval_equality_operator NotEqual eval1 eval2

        | Or -> eval_junction_operator Or eval1 eval2
        | And -> eval_junction_operator And eval1 eval2
      )
    | If (ex1, ex2, ex3) -> (
      match (eval_expr env ex1) with
      | Bool true -> (eval_expr env ex2)
      | Bool false -> (eval_expr env ex3)
      | _ -> raise (TypeError "Expected Bool")
    )
    | FunctionCall (ex1, ex2) -> 
      (match (eval_expr env ex1) with
        | Closure (cenv, cvar, cexpr) -> 
          let new_value = eval_expr env ex2 in
          eval_expr (extend cenv cvar new_value) cexpr
        | _ -> raise (TypeError "Expected Closure")
      )
    | Let (id, recursive, ex1, ex2) ->
      match recursive with
      | true -> let new_env = (extend_tmp env id) in
        let new_value = eval_expr new_env ex1 in
        update new_env id new_value;
        eval_expr new_env ex2
      | false -> let new_value = (eval_expr env ex1) in
        eval_expr (extend env id new_value) ex2  

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | Def (vari, ex) -> 
    let new_env = (extend_tmp env vari) in
    let v = eval_expr new_env ex in 
    update new_env vari v;
    (new_env, Some v)
  | Expr ex ->
    let v = eval_expr env ex in
    (env, Some v)
  | NoOp -> (env, None)