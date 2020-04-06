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

  let assert_function (f : t) = match f with
  | Lambda(ff) -> ff
  | _ -> raise (Exn "Invalid function application")

  let check_symbol (s : string) =
    if Hashtbl.mem functions s then
      Hashtbl.find functions s
    else
      Symbol(s)
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
  let c = match [%e compile env cond] with
    | Shen.Boolean(true) -> true
    | _ -> false
  in
  if c then
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