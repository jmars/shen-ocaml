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

  let functions: (string, Obj.t) Hashtbl.t = Hashtbl.create 255
  let globals: (string, t) Hashtbl.t = Hashtbl.create 255

  let assert_function: 'a -> t -> t = fun x -> match (Obj.repr x) with
    | x when (Obj.tag x) = Obj.closure_tag -> x |> Obj.obj
    | _ -> raise (Exn "Invalid function application")

  let define n f = Hashtbl.add functions n (Obj.repr f)

  let assert_bool (s : t) = match s with
    | Boolean(b) -> b
    | _ -> raise (Exn "Expected a bool")
end

module Env = Map.Make(String)

type func
  = Ref of Obj.t Ppx_stage.code
  | Func of Obj.t Ppx_stage.code

type env = {
  lexical: Shen.t Ppx_stage.code Env.t;
  functions: func Env.t;
}

let empty_env = {
  lexical = Env.empty;
  functions = Env.empty;
}

let rec compile (env : env) (expr : ast)
  = match expr with
  | Boolean(v) -> [%code Shen.Boolean([%e Ppx_stage.Lift.bool v])]
  | Number(v) -> [%code Shen.Number([%e Ppx_stage.Lift.float v])]
  | Symbol(v) ->
    if Env.mem v env.lexical then
      [%code [%e Env.find v env.lexical]]
    else
      [%code Shen.Symbol([%e Ppx_stage.Lift.string v])]
  | String(v) -> [%code Shen.Symbol([%e Ppx_stage.Lift.string v])]
  | If(cond, ifTrue, ifFalse) -> [%code
    if Shen.assert_bool [%e compile env cond] then
      [%e compile env ifTrue]
    else
      [%e compile env ifFalse]
  ]
  | Let(bind, v, body) -> [%code
    let x = [%e compile env v] in
    [%e compile
      {env with lexical = (Env.add bind [%code x] env.lexical)} body]
  ]
  | Lambda(arg, body) -> [%code
    fun a -> [%e compile
      {env with lexical = (Env.add arg [%code a] env.lexical) } body]]
      |> Obj.magic
  | App(Symbol(f), a) when (Env.mem f env.functions) -> [%code
    [%e match Env.find f env.functions with
      | Func(f) -> Obj.magic f
      | Ref(f) -> [%code ![%e f |> Obj.magic]]] [%e compile env a]
  ]
  | App(f, a) -> [%code
    [%e compile env f |> Obj.magic] [%e compile env a]
  ]
  | Trap(body, handler) -> [%code
    try [%e compile env body]
    with Shen.Exn(_) as e ->
      Shen.assert_function [%e compile env handler] (Shen.Error e)
  ]

let rec find_forward_calls (env : func Env.t) (expr : ast)
= match expr with
| If(cond, ifTrue, ifFalse) ->
  find_forward_calls env cond
  @ find_forward_calls env ifTrue
  @ find_forward_calls env ifFalse
| Let(bind, v, body) ->
  find_forward_calls env v
  @ find_forward_calls env body
| Lambda(arg, body) ->
  find_forward_calls env body
| App(Symbol(f), a) when (not (Env.mem f env)) ->
  f :: find_forward_calls env a
| App(f, a) ->
  find_forward_calls env f
  @ find_forward_calls env a
| Trap(body, handler) ->
  find_forward_calls env body
  @ find_forward_calls env handler
| _ -> []

let rec lambda_form (params : string list) (env : env) (body : ast)
  : (Shen.t -> Shen.t) Ppx_stage.code
  = match params with
  | p :: [] -> [%code
    fun x -> [%e compile
      { env with lexical = Env.add p [%code x] env.lexical }
      body
    ]
  ]
  | p :: rest -> [%code
    fun x -> [%e lambda_form rest
      { env with lexical = Env.add p [%code x] env.lexical }
      body
    |> Obj.magic]
  ]
| [] -> compile env body |> Obj.magic

module FSet = Map.Make(String)

let rec compile_toplevel
(env : func Env.t) (exprs : toplevel list) = match exprs with
  | (Defun(name, (p :: params), body) :: rest) as all ->
  let fcalls = find_forward_calls env body in
  if (List.length fcalls) = 0 then
    if Env.mem name env then
      match Env.find name env with
      | Func(_) -> assert false
      | Ref(f) -> let f = Obj.magic f in [%code
        [%e f] := (fun x ->
          [%e lambda_form
            params
            {
              lexical = Env.add p [%code x] Env.empty;
              functions = env
            }
            body
          ]);
          Shen.define [%e Ppx_stage.Lift.string name] ![%e f] |> ignore;
          [%e compile_toplevel env rest]
      ]
    else
      [%code
        let rec f = fun x ->
          [%e lambda_form
            params
            { 
              lexical = Env.add p [%code x] Env.empty;
              functions = Env.add name (Func((Obj.magic [%code f]))) env
            }
            body
          ]
        in
        Shen.define [%e Ppx_stage.Lift.string name] f |> ignore;
        [%e compile_toplevel (Env.add name (Func(Obj.magic [%code f])) env) rest]
      ]
  else
    [%code
      let rec f = ref (fun x -> x) |> Obj.magic in
      [%e compile_toplevel
        (Env.add
          (List.hd fcalls) (Ref(Obj.magic [%code f])) env) all]
    ]
  | Expr(body) :: rest -> [%code
    [%e compile { lexical = Env.empty; functions = env } body] |> ignore;
    [%e compile_toplevel env rest]
  ]
  | [] -> [%code ()]
  | _ -> assert false