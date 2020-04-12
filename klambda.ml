open Reader
open Kparser
open Compiler

let _ =
  let sexps = Reader.read_kl "test.kl" in
  let ast = Kparser.file_to_kl sexps in
  (* List.iter (fun x -> print_endline (show_toplevel x)) ast; *)
  let compiled = compile_toplevel Env.empty ast in
  Ppx_stage.print Format.std_formatter compiled;
  ()