open Str
open Sexplib

let read_file p =
  let ch = open_in p in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let catch_string = regexp "\"[^\"]*\""

let read_kl p =
  let str = read_file p in
  let ss =
    ("(" ^ str ^ ")") |>
    global_replace catch_string "(str \0)"
  in
  let sexps = Sexp.of_string ss in
  match sexps with
  | List(ss) -> ss
  | _ -> assert false
