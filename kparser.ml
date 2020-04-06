open Sexplib

let is_float v = match float_of_string_opt v with
  | Some(_) -> true
  | None -> false

let i = ref 0
let gensym () =
  let r = "x" ^ string_of_int !i in
  i := !i + 1;
  r

type ast =
  | Boolean of bool
  | Number of float
  | Symbol of string
  | String of string
  | App of ast * ast
  | Lambda of string * ast
  | Let of string * ast * ast
  | Trap of ast * ast
  | If of ast * ast * ast
[@@deriving show]

let rec sexp_to_kl (sexp : Sexp.t) = match sexp with
  | Atom("true") -> Boolean(true)
  | Atom("false") -> Boolean(false)
  | Atom(v) when (is_float v) -> Number(float_of_string v)
  | Atom(atom) -> Symbol(atom)
  | List([Atom("str"); Atom(v)]) -> String(v)
  | List([Atom("and"); a; b]) ->
      If(sexp_to_kl a, sexp_to_kl b, Boolean(false))
  | List([Atom("or"); a; b]) ->
      If(sexp_to_kl a, Boolean(true), sexp_to_kl b)
  | List([Atom("let"); Atom(n); v; b]) ->
    Let(n, sexp_to_kl v, sexp_to_kl b)
  | List([Atom("lambda"); Atom(arg); b]) ->
    Lambda(arg, sexp_to_kl b)
  | List([Atom("freeze"); v]) -> Lambda(gensym(), sexp_to_kl v)
  | List([Atom("thaw"); v]) -> App(Symbol("thaw"), Number(0.0))
  | List([Atom("cond")]) -> App(Symbol("simple-error"), String("case failure"))
  | List(Atom("cond") :: List([p; q]) :: r) ->
    sexp_to_kl (List([Atom("if"); p; q; List(Atom("cond") :: r)]))
  | List([Atom("do"); x]) -> sexp_to_kl x
  | List(Atom("do") :: x :: z) ->
    Let(gensym(), sexp_to_kl x, sexp_to_kl (List(Atom("do") :: z)))
  | List(r :: x :: y :: z) ->
     sexp_to_kl (List(List([r; x]) :: y :: z))
  | List([x; y]) -> App(sexp_to_kl x, sexp_to_kl y)
  | List([x]) -> App(sexp_to_kl x, Number(0.0))
  | List([]) -> assert false

type toplevel =
  | Defun of string * string list * ast
  | Expr of ast
[@@deriving show]

let extract_symbol (s : Sexp.t) = match s with
| Atom(x) -> x
| _ -> assert false

let toplevel_to_kl (sexp : Sexp.t) = match sexp with
  | List([Atom("defun"); Atom(name); List(args); body]) ->
    Defun(name, (List.map extract_symbol args), sexp_to_kl body)
  | x -> Expr(sexp_to_kl x)

let file_to_kl (sexps : Sexp.t list) =
  List.map toplevel_to_kl sexps