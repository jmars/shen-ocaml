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
    let functions : (string, t) Hashtbl.t = Hashtbl.create 255
    let assert_function (f : t) =
      match f with
      | Lambda ff -> ff
      | _ -> raise (Exn "Invalid function application")
    let check_symbol (s : string) =
      if Hashtbl.mem functions s then Hashtbl.find functions s else Symbol s
  end
let _ =
  let _ =
    Shen'1.Lambda
      (fun a ->
         let x = Shen'1.Number 1. in
         (Shen'1.assert_function
            ((Shen'1.assert_function
                ((Shen'1.assert_function (Shen'1.check_symbol "+")) x)) a))
           (Shen'1.Symbol "2 birds")) in
  let _ =
    Hashtbl.add Shen'1.functions "add"
      (Shen'1.Lambda
         (fun a ->
            let a''1 = a in
            Shen'1.Lambda
              (fun a ->
                 (Shen'1.assert_function
                    ((Shen'1.assert_function (Shen'1.check_symbol "+")) a''1))
                   a))) in
  ()