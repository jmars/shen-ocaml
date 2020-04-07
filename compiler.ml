open Kparser

module%code Shen = struct[@code]
  exception Exn of string

  type t =
    | Symbol of string
    | String of string
    | Boolean of bool
    | Number of float
    | Error of exn
    | Vector of t array
    | List of t list
    | Lambda of (t -> t)
    | Stream of in_channel * out_channel
  
  let functions: (string, t) Hashtbl.t = Hashtbl.create 255
  let globals: (string, t) Hashtbl.t = Hashtbl.create 255

  let assert_function (f : t) = match f with
  | Lambda(ff) -> ff
  | _ -> raise (Exn "Invalid function application")

  let check_symbol (s : string) =
    if Hashtbl.mem functions s then
      Hashtbl.find functions s
    else
      Symbol(s)
  
  let assert_bool (s : t) = match s with
  | Boolean(b) -> b
  | _ -> raise (Exn "Expected a bool")

  let kl_if (c : t) = Lambda(fun (it : t) -> Lambda(fun (iff : t) ->
    if (assert_bool c) then
      it
    else
      iff))
  
  let kl_or (l : t) = Lambda(fun (r : t) ->
    if assert_bool l then (Boolean true) else r )

  let kl_and (l : t) = Lambda(fun (r : t) ->
    if assert_bool l then r else (Boolean false))

  let kl_simple_error (v : t) = match v with
  | String(s) -> raise (Exn s)
  | _ -> raise (Exn "simple-error: first arg must be a string")

  let kl_error_to_string (v : t) = match v with
  | Error(Exn(m)) -> m
  | _ -> raise (Exn "error-to-string: first arg must be an error")

  let kl_intern (v : t) = match v with
  | String("true") -> Boolean(true)
  | String("false") -> Boolean(false)
  | String(s) -> Symbol(s)
  | _ -> raise (Exn "intern: argument must be a string")

  let kl_set (n : t) = Lambda(fun (v : t) ->
    match n with
    | Symbol(k) -> Hashtbl.add globals k v; v
    | _ -> raise (Exn "set: first arg must be a symbol"))
  
  let kl_value (n : t) = match n with
  | Symbol(k) -> Hashtbl.find globals k
  | _ -> raise (Exn "value: first arg must be a symbol")

  let kl_is_number (v : t) = match v with
  | Number(_) -> (Boolean true)
  | _ -> (Boolean false)

  let kl_arith (f : float -> float -> float) (a : t) = Lambda(fun (b : t) ->
    match (a, b) with
    | (Number v1, Number v2) -> Number(f v1 v2)
    | _ -> raise (Exn "+-/*: both arguments must be numbers"))

  let kl_add = kl_arith (fun a b -> a +. b)
  let kl_min = kl_arith (fun a b -> a -. b)
  let kl_mul = kl_arith (fun a b -> a *. b)
  let kl_div = kl_arith (fun a b -> a /. b)

  let kl_cmp (f : float -> float -> bool) (a : t) = Lambda(fun (b : t) ->
    match (a, b) with
    | (Number v1, Number v2) -> Boolean(f v1 v2)
    | _ -> raise (Exn ">=<=: both arguments must be numbers"))
  
  let kl_gt = kl_cmp (fun a b -> a > b)
  let kl_lt = kl_cmp (fun a b -> a < b)
  let kl_lte = kl_cmp (fun a b -> a <= b)
  let kl_gte = kl_cmp (fun a b -> a >= b)
end

module Env = Map.Make(String)

let rec compile (env : Shen.t Ppx_stage.code Env.t) (expr : ast)
= match expr with
| Boolean(v) -> [%code Shen.Boolean([%e Ppx_stage.Lift.bool v])]
| Number(v) -> [%code Shen.Number([%e Ppx_stage.Lift.float v])]
| Symbol(v) ->
  if Env.mem v env then
    [%code [%e Env.find v env]]
  else
    [%code Shen.check_symbol [%e Ppx_stage.Lift.string v]]
| String(v) -> [%code Shen.Symbol([%e Ppx_stage.Lift.string v])]
| If(cond, ifTrue, ifFalse) -> [%code
  if Shen.assert_bool [%e compile env cond] then
    [%e compile env ifTrue]
  else
    [%e compile env ifFalse]
]
| Let(bind, v, body) -> [%code
  let x = [%e compile env v] in
  [%e compile (Env.add bind [%code x] env) body]
]
| Lambda(arg, body) -> [%code
  Shen.Lambda(fun a -> [%e compile (Env.add arg [%code a] env) body])]
| App(f, a) -> [%code
  (Shen.assert_function [%e compile env f]) [%e compile env a]
]
| Trap(body, handler) -> [%code
  try [%e compile env body]
  with Shen.Exn(_) as e ->
  Shen.assert_function [%e compile env handler] (Shen.Error e)
]

let rec lambda_form (params : string list) (body : ast)
= match params with
  | p :: rest -> Lambda(p, lambda_form rest body)
  | [] -> body

let rec compile_toplevel (exprs : toplevel list) = match exprs with
  | Defun(name, params, body) :: rest -> [%code
    let _ = 
      Hashtbl.add
        Shen.functions
        [%e Ppx_stage.Lift.string name]
        [%e lambda_form params body |> compile Env.empty]
    in
      [%e compile_toplevel rest]
  ]
  | Expr(body) :: rest -> [%code
    let _ = [%e compile Env.empty body] in
    [%e compile_toplevel rest]
  ]
  | [] -> [%code ()]