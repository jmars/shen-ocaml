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
    let globals : (string, t) Hashtbl.t = Hashtbl.create 255
    let assert_function (f : t) =
      match f with
      | Lambda ff -> ff
      | _ -> raise (Exn "Invalid function application")
    let check_symbol (s : string) =
      if Hashtbl.mem functions s then Hashtbl.find functions s else Symbol s
    let assert_bool (s : t) =
      match s with | Boolean b -> b | _ -> raise (Exn "Expected a bool")
    let kl_if (c : t) =
      Lambda
        (fun (it : t) ->
           Lambda (fun (iff : t) -> if assert_bool c then it else iff))
    let kl_or (l : t) =
      Lambda (fun (r : t) -> if assert_bool l then Boolean true else r)
    let kl_and (l : t) =
      Lambda (fun (r : t) -> if assert_bool l then r else Boolean false)
    let kl_simple_error (v : t) =
      match v with
      | String s -> raise (Exn s)
      | _ -> raise (Exn "simple-error: first arg must be a string")
    let kl_error_to_string (v : t) =
      match v with
      | Error (Exn m) -> m
      | _ -> raise (Exn "error-to-string: first arg must be an error")
    let kl_intern (v : t) =
      match v with
      | String "true" -> Boolean true
      | String "false" -> Boolean false
      | String s -> Symbol s
      | _ -> raise (Exn "intern: argument must be a string")
    let kl_set (n : t) =
      Lambda
        (fun (v : t) ->
           match n with
           | Symbol k -> (Hashtbl.add globals k v; v)
           | _ -> raise (Exn "set: first arg must be a symbol"))
    let kl_value (n : t) =
      match n with
      | Symbol k -> Hashtbl.find globals k
      | _ -> raise (Exn "value: first arg must be a symbol")
    let kl_is_number (v : t) =
      match v with | Number _ -> Boolean true | _ -> Boolean false
    let kl_arith (f : float -> float -> float) (a : t) =
      Lambda
        (fun (b : t) ->
           match (a, b) with
           | (Number v1, Number v2) -> Number (f v1 v2)
           | _ -> raise (Exn "+-/*: both arguments must be numbers"))
    let kl_add = kl_arith (fun a -> fun b -> a +. b)
    let kl_min = kl_arith (fun a -> fun b -> a -. b)
    let kl_mul = kl_arith (fun a -> fun b -> a *. b)
    let kl_div = kl_arith (fun a -> fun b -> a /. b)
    let kl_cmp (f : float -> float -> bool) (a : t) =
      Lambda
        (fun (b : t) ->
           match (a, b) with
           | (Number v1, Number v2) -> Boolean (f v1 v2)
           | _ -> raise (Exn ">=<=: both arguments must be numbers"))
    let kl_gt = kl_cmp (fun a -> fun b -> a > b)
    let kl_lt = kl_cmp (fun a -> fun b -> a < b)
    let kl_lte = kl_cmp (fun a -> fun b -> a <= b)
    let kl_gte = kl_cmp (fun a -> fun b -> a >= b)
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