open Reader
open Compiler

let _ =
  let sexps = Reader.read_kl Sys.argv.(1) in
  let ast = Compiler.Parser.file_to_kl sexps in
  (* List.iter (fun x -> print_endline (Compiler.Parser.show_toplevel x)) ast; *)
  let compiled = Compiler.compile_toplevel Env.empty ast in
  Ppx_stage.print Format.std_formatter compiled;
  ()