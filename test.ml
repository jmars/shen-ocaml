module Shen'1 =
  struct
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
    let functions : (string, Obj.t) Hashtbl.t = Hashtbl.create 255
    let globals : (string, t) Hashtbl.t = Hashtbl.create 255
    let assert_function : 'a -> t -> t =
      fun x ->
        match Obj.repr x with
        | x when (Obj.tag x) = Obj.closure_tag -> x |> Obj.obj
        | _ -> raise (Exn "Invalid function application")
    let define n f = Hashtbl.add functions n (Obj.repr f)
    let sfunction (s : string) = Hashtbl.find functions s
    let check_symbol (s : string) =
      if Hashtbl.mem functions s
      then Hashtbl.find functions s
      else (Symbol s) |> Obj.repr
    let assert_bool (s : t) =
      match s with | Boolean b -> b | _ -> raise (Exn "Expected a bool")
  end
let _ =
  let _ = (fun x -> fun x -> x) |> (Shen'1.define "add") in
  let _ =
    (Obj.obj (Shen'1.sfunction "add") (Shen'1.Number 1.)) (Shen'1.Number 2.) in
  ()