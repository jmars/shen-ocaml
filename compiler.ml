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

  let sfunction (s : string) = Hashtbl.find functions s

  let check_symbol (s : string) =
    if Hashtbl.mem functions s then
      Hashtbl.find functions s 
    else
      Symbol(s) |> Obj.repr

  let assert_bool (s : t) = match s with
    | Boolean(b) -> b
    | _ -> raise (Exn "Expected a bool")
end

module Env = Map.Make(String)

type env = {
  lexical: Shen.t Ppx_stage.code Env.t;
  functions: int Env.t;
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
      [%code Shen.check_symbol [%e Ppx_stage.Lift.string v] |> Obj.obj]
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
    Obj.obj [%e [%code Shen.sfunction [%e Ppx_stage.Lift.string f]]
    |> Obj.magic] [%e compile env a]
  ]
  | App(f, a) -> [%code
    [%e compile env f |> Obj.magic] [%e compile env a]
  ]
  | Trap(body, handler) -> [%code
    try [%e compile env body]
    with Shen.Exn(_) as e ->
      Shen.assert_function [%e compile env handler] (Shen.Error e)
  ]

let rec lambda_form (params : string list) (env : env) (body : ast)
  : Shen.t Ppx_stage.code
  = match params with
  | p :: [] -> [%code
    fun x -> [%e compile
      { env with lexical = Env.add p [%code x] env.lexical }
      body
    ]
  ] |> Obj.magic
  | p :: rest -> [%code
    fun x -> [%e lambda_form rest
      { env with lexical = Env.add p [%code x] env.lexical }
      body
    ]
  ] |> Obj.magic
  | [] -> assert false

let rec compile_toplevel
(env : int Env.t) (exprs : toplevel list) = match exprs with
  | Defun(name, params, body) :: rest -> [%code
    let _ = 
      [%e lambda_form
        params
        { lexical = Env.empty; functions = env }
        body
      ] |> Shen.define [%e Ppx_stage.Lift.string name]
    in
    [%e compile_toplevel (Env.add name (List.length params) env) rest]
  ]
  | Expr(body) :: rest -> [%code
    let _ = [%e compile
      { lexical = Env.empty; functions = env } body] in
    [%e compile_toplevel env rest]
  ]
  | [] -> [%code ()]