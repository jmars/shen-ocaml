open Str
open Sexplib

let read_file p =
  let ch = open_in p in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let catch_string = regexp "\"[^\"]*\""
let bad_semi = regexp ";"

let rec sexp_to_variant (s : Sexp.t) = match s with
  | Atom(a) -> `Atom(a)
  | List(l) -> `List(List.map sexp_to_variant l)

let read_kl p =
  let str = read_file p in
  let ss =
    ("(" ^ str ^ ")") |>
    global_replace catch_string "(str \0)" |>
    global_replace bad_semi "(intern (n->string 59))"
  in
  let sexps = Sexp.of_string ss in
  match sexps with
  | List(ss) -> List.map sexp_to_variant ss
  | _ -> assert false
