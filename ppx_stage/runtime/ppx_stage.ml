module Internal = Internal
module IdentMap = IdentMap

type 'a code = {
  compute : Internal.Environ.t -> 'a;
  source : Internal.Renaming.t -> Internal.dynamic_modcontext -> Parsetree.expression;
}

let to_parsetree_structure f =
  Internal.generate_source f.source
  |> Internal.to_structure

(* let to_parsetree f = f.source Internal.Renaming.empty *)

let run f = f.compute Internal.Environ.empty

let print ppf f =
  Pprintast.structure ppf (to_parsetree_structure f)



module Lift = struct
  open Parsetree
  open Ast_helper

  let lift c p = { compute = (fun _ -> c); source = (fun _ _ -> p) }

  let int x : int code =
    lift x (Exp.constant (Pconst_integer (string_of_int x, None)))
  let int32 x : Int32.t code =
    lift x (Exp.constant (Pconst_integer (Int32.to_string x, Some 'l')))
  let int64 x : Int64.t code =
    lift x (Exp.constant (Pconst_integer (Int64.to_string x, Some 'L')))
  let bool x : bool code =
    lift x (Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool x))) None)
  let float x : float code =
    (* OCaml's string_of_float is a bit broken *)
    let s = string_of_float x in
    if float_of_string s = x then
      lift x (Exp.constant (Pconst_float (s, None)))
    else
      lift x (Exp.constant (Pconst_float (Printf.sprintf "%h" x, None)))
  let string x : string code =
    lift x (Exp.constant (Pconst_string (x, None)))
end


type staged_module = Parsetree.module_expr
