module Parser'1 =
  struct
    let is_float v =
      try (float_of_string v) |> ignore; true with | _ -> false
    let i = ref 0
    let gensym () = let r = "x" ^ (string_of_int (!i)) in i := ((!i) + 1); r
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
      | Empty [@@deriving show]
    let rec sexp_to_kl sexp =
      match sexp with
      | `Atom "true" -> Boolean true
      | `Atom "false" -> Boolean false
      | `Atom v when is_float v -> Number (float_of_string v)
      | `Atom atom -> Symbol atom
      | `List [] -> Empty
      | `List ((`Atom "str")::(`Atom v)::[]) -> String v
      | `List ((`Atom "if")::c::a::b::[]) ->
          If ((sexp_to_kl c), (sexp_to_kl a), (sexp_to_kl b))
      | `List ((`Atom "and")::a::b::[]) ->
          If ((sexp_to_kl a), (sexp_to_kl b), (Boolean false))
      | `List ((`Atom "or")::a::b::[]) ->
          If ((sexp_to_kl a), (Boolean true), (sexp_to_kl b))
      | `List ((`Atom "let")::(`Atom n)::v::b::[]) ->
          Let (n, (sexp_to_kl v), (sexp_to_kl b))
      | `List ((`Atom "lambda")::(`Atom arg)::b::[]) ->
          Lambda (arg, (sexp_to_kl b))
      | `List ((`Atom "freeze")::v::[]) ->
          Lambda ((gensym ()), (sexp_to_kl v))
      | `List ((`Atom "thaw")::v::[]) ->
          App ((Symbol "thaw"), (sexp_to_kl v))
      | `List ((`Atom "cond")::[]) ->
          App ((Symbol "simple-error"), (String "case failure"))
      | `List ((`Atom "cond")::(`List (p::q::[]))::r) ->
          sexp_to_kl (`List [`Atom "if"; p; q; `List ((`Atom "cond") :: r)])
      | `List ((`Atom "trap-error")::body::lam::[]) ->
          Trap ((sexp_to_kl body), (sexp_to_kl lam))
      | `List ((`Atom "do")::x::[]) -> sexp_to_kl x
      | `List ((`Atom "do")::x::z) ->
          Let
            ((gensym ()), (sexp_to_kl x),
              (sexp_to_kl (`List ((`Atom "do") :: z))))
      | `List (r::x::y::z) -> sexp_to_kl (`List ((`List [r; x]) :: y :: z))
      | `List (x::y::[]) -> App ((sexp_to_kl x), (sexp_to_kl y))
      | `List (x::[]) -> App ((sexp_to_kl x), (Number 0.0))
    type toplevel =
      | Defun of string * string list * ast 
      | Expr of ast [@@deriving show]
    let extract_symbol s = match s with | `Atom x -> x | _ -> assert false
    let toplevel_to_kl sexp =
      match sexp with
      | `List ((`Atom "defun")::(`Atom name)::(`List args)::body::[]) ->
          Defun (name, (List.map extract_symbol args), (sexp_to_kl body))
      | x -> Expr (sexp_to_kl x)
    let file_to_kl sexps = List.map toplevel_to_kl sexps
  end
module Shen'1 =
  struct
    let is_bucklescript = false
    exception Exn of string 
    type t =
      | Symbol of string 
      | String of string 
      | Boolean of bool 
      | Number of float 
      | Error of exn 
      | Vector of t array 
      | Cons of t * t 
      | Empty 
      | Stream of in_channel * out_channel 
    let functions : (string, Obj.t) Hashtbl.t = Hashtbl.create 255
    let globals : (string, t) Hashtbl.t = Hashtbl.create 255
    let assert_function : 'a -> t -> t =
      fun x ->
        match Obj.repr x with
        | x when (Obj.tag x) = Obj.closure_tag -> x |> Obj.obj
        | _ -> raise (Exn "Invalid function application")
    let define n f = Hashtbl.add functions n (Obj.repr f)
    let assert_bool (s : t) =
      match s with | Boolean b -> b | _ -> raise (Exn "Expected a bool")
    let thaw (a : unit -> t) =
      if is_bucklescript
      then
        let f : unit -> t = Obj.magic a in
        try f () with | _ -> raise (Exn "Attempt to thaw non-frozen")
      else
        if (Obj.tag (Obj.repr a)) = Obj.closure_tag
        then (let f : unit -> t = Obj.magic a in f ())
        else raise (Exn "Attempt to thaw non-frozen")
    let simple_error (a : t) =
      match a with
      | String s -> raise (Exn s)
      | _ -> raise (Exn "simple-error: argument must be a string")
    let error_to_string (a : t) =
      match a with
      | Error (Exn s) -> String s
      | _ -> raise (Exn "error-to-string: argument must be an error")
    let intern (a : t) =
      match a with
      | String "true" -> Boolean true
      | String "false" -> Boolean false
      | String s -> Symbol s
      | _ -> raise (Exn "intern: first argument must be a string")
    let set (name : t) (value : t) =
      match name with
      | Symbol s -> (Hashtbl.add globals s value; value)
      | _ -> raise (Exn "set: first argument must be a symbol")
    let () =
      set (Symbol "*stinput*") (Stream (stdin, stdout));
      set (Symbol "*stoutput*") (Stream (stdin, stdout));
      set (Symbol "*sterror*") (Stream (stdin, stderr));
      set (Symbol "*home-directory*") (String "/home/jaye");
      set (Symbol "*language*") (String "OCaml");
      set (Symbol "*implementation*") (String "4.05");
      set (Symbol "*os*") (String "Linux");
      set (Symbol "*port") (String "0.1");
      set (Symbol "*porters*")
        (String "Jaye Marshall <marshall.jaye@gmail.com>");
      ()
    let value (name : t) =
      match name with
      | Symbol s ->
          (try Hashtbl.find globals s
           with | Not_found -> raise (Exn "value: no value"))
      | _ -> raise (Exn "value: first argument must be a symbol")
    let is_number (n : t) =
      match n with | Number _ -> Boolean true | _ -> Boolean false
    let add (a : t) (b : t) =
      match (a, b) with
      | (Number a, Number b) -> Number (a +. b)
      | _ -> raise (Exn "+: both arguments must be numbers")
    let sub (a : t) (b : t) =
      match (a, b) with
      | (Number a, Number b) -> Number (a -. b)
      | _ -> raise (Exn "-: both arguments must be numbers")
    let mul (a : t) (b : t) =
      match (a, b) with
      | (Number a, Number b) -> Number (a *. b)
      | _ -> raise (Exn "*: both arguments must be numbers")
    let div (a : t) (b : t) =
      match (a, b) with
      | (Number a, Number b) -> Number (a /. b)
      | _ -> raise (Exn "/: both arguments must be numbers")
    let gt (a : t) (b : t) =
      match (a, b) with
      | (Number a, Number b) -> Boolean (a > b)
      | _ -> raise (Exn ">: both arguments must be numbers")
    let lt (a : t) (b : t) =
      match (a, b) with
      | (Number a, Number b) -> Boolean (a < b)
      | _ -> raise (Exn "<: both arguments must be numbers")
    let gte (a : t) (b : t) =
      match (a, b) with
      | (Number a, Number b) -> Boolean (a >= b)
      | _ -> raise (Exn ">=: both arguments must be numbers")
    let lte (a : t) (b : t) =
      match (a, b) with
      | (Number a, Number b) -> Boolean (a <= b)
      | _ -> raise (Exn "<=: both arguments must be numbers")
    let is_string (a : t) =
      match a with | String _ -> Boolean true | _ -> Boolean false
    let pos (str : t) (i : t) =
      match (str, i) with
      | (String str, Number i) ->
          (try
             String (String.sub str (int_of_float i) ((int_of_float i) + 1))
           with | Invalid_argument _ -> raise (Exn "pos: invalid index"))
      | _ -> raise (Exn "pos: args must be string -> number")
    let tlstr (str : t) =
      match str with
      | String "" -> raise (Exn "tlstr on empty string")
      | String str -> String (String.sub str 1 ((String.length str) - 1))
      | _ -> raise (Exn "tlstr: argument must be a string")
    let cn (a : t) (b : t) =
      match (a, b) with
      | (String a, String b) -> String (a ^ b)
      | _ -> raise (Exn "cn: both arguments must be strings")
    let rec str (a : t) =
      match a with
      | Symbol s -> String s
      | String _ as s -> s
      | Boolean (true) -> String "true"
      | Boolean (false) -> String "false"
      | Number n -> String (string_of_float n)
      | Error (Exn n) -> String n
      | Vector v ->
          String ("<vector " ^ ((string_of_int (Array.length v)) ^ ">"))
      | Cons (a, b) ->
          (match ((str a), (str b)) with
           | (String a, String b) ->
               String ("[cons " ^ (a ^ (" " ^ (b ^ "]"))))
           | _ -> raise (Exn "str: invalid argument"))
      | Empty -> String "[]"
      | Stream _ -> String "stream"
      | _ -> raise (Exn "str: invalid argument")
    let string_to_n (str : t) =
      match str with
      | String str ->
          (try Number ((Char.code (str.[0])) |> float_of_int)
           with | Invalid_argument _ -> raise (Exn "string->n: empty string"))
      | _ -> raise (Exn "string->n: arg must be a string")
    let n_to_string (num : t) =
      match num with
      | Number num ->
          String ((Char.chr (int_of_float num)) |> (String.make 1))
      | _ -> raise (Exn "n->string: arg must be a number")
    let absvector (num : t) =
      match num with
      | Number num -> Vector (Array.make (int_of_float num) (Symbol "nil"))
      | _ -> raise (Exn "absvector: arg must be a number")
    let is_cons (v : t) =
      match v with | Cons (_, _) -> Boolean true | _ -> Boolean false
    let cons (v : t) (l : t) = Cons (v, l)
    let hd (l : t) =
      match l with
      | Empty -> raise (Exn "hd: empty list")
      | Cons (v, l) -> v
      | _ -> raise (Exn "hd: arg must be a list")
    let tl (l : t) =
      match l with
      | Empty -> raise (Exn "tl: empty list")
      | Cons (v, l) -> l
      | _ -> raise (Exn "tl: arg must be a list")
    let write_byte (byte : t) (stream : t) =
      match (byte, stream) with
      | (Number byte, Stream (_, stream)) ->
          (output_byte stream (int_of_float byte); Number byte)
      | _ -> raise (Exn "write-byte: args must be number -> stream")
    let read_byte (stream : t) =
      match stream with
      | Stream (s, _) -> Number ((input_byte s) |> float_of_int)
      | _ -> raise (Exn "read-byte: arg must be a string")
    let open_stream (path : t) (config : t) =
      match (path, config) with
      | (String path, Symbol config) ->
          Stream ((open_in_bin path), (open_out_bin path))
      | _ -> raise (Exn "open: args must be string -> symbol")
    let close_stream (stream : t) =
      match stream with
      | Stream (in_, out) -> (close_in in_; close_out out; Empty)
      | _ -> raise (Exn "close: arg must be a stream")
    let eq (a : t) (b : t) = Boolean (a = b)
    let start_time = Unix.time ()
    let get_time (a : t) =
      match a with
      | Symbol "unix" -> Number (Unix.time ())
      | Symbol _ -> Number ((Unix.time ()) -. start_time)
      | _ -> raise (Exn "get-time: invalid arg")
    let is_boolean (a : t) =
      match a with | Boolean _ -> Boolean true | _ -> Boolean false
    let is_symbol (a : t) =
      match a with
      | x when
          (not is_bucklescript) &&
            ((Obj.tag (Obj.repr x)) == Obj.closure_tag)
          -> Boolean false
      | Symbol _ -> Boolean true
      | _ -> Boolean false
    let rec cons_to_sexp c =
      match c with
      | Empty -> []
      | Cons (h, t) -> (value_to_sexp h) :: (cons_to_sexp t)
      | _ -> assert false
    and value_to_sexp (v : t) =
      match v with
      | Symbol s -> `Atom s
      | String s -> `List [`Atom "str"; `Atom s]
      | Boolean b -> `Atom (string_of_bool b)
      | Number n -> `Atom (string_of_float n)
      | Cons (h, t) -> `List ((value_to_sexp h) :: (cons_to_sexp t))
      | _ -> raise (Exn "Invalid value in eval")
    module EvalEnv = (Map.Make)(String)
    let rec eval (env : t EvalEnv.t) (e : Parser'1.ast) =
      (match e with
       | Symbol s ->
           if EvalEnv.mem s env then EvalEnv.find s env else Symbol s
       | String s -> String s
       | Boolean b -> Boolean b
       | Number n -> Number n
       | App (Symbol f, a) ->
           let f : t -> t = (Hashtbl.find functions f) |> Obj.obj in
           f (eval env a)
       | App (f, a) ->
           let f : t -> t = (eval env f) |> Obj.magic in
           let a = eval env a in f a
       | Lambda (a, b) ->
           (fun av -> let benv = EvalEnv.add a av env in eval benv b) |>
             Obj.magic
       | Let (n, v, b) ->
           let benv = EvalEnv.add n (eval env v) env in eval benv b
       | If (c, t, f) ->
           (match eval env c with
            | Boolean (true) -> eval env t
            | Boolean (false) -> eval env f
            | _ -> raise (Exn "Invalid if condition - must be a boolean"))
       | Trap (b, h) ->
           (try eval env b
            with
            | Exn _ as e ->
                let h : t -> t = Obj.magic (eval env h) in h (Error e))
       | _ -> raise (Exn "eval error") : t)
    let eval_kl (e : t) =
      match e with
      | Cons (_, _) as l ->
          let sexp = value_to_sexp l in
          let ast = Parser'1.sexp_to_kl sexp in eval EvalEnv.empty ast
      | _ -> raise (Exn "eval-kl: invalid expression")
    let () =
      Hashtbl.add functions "thaw" (Obj.repr thaw);
      Hashtbl.add functions "simple-error" (Obj.repr simple_error);
      Hashtbl.add functions "error-to-string" (Obj.repr error_to_string);
      Hashtbl.add functions "simple-error" (Obj.repr simple_error);
      Hashtbl.add functions "intern" (Obj.repr intern);
      Hashtbl.add functions "set" (Obj.repr set);
      Hashtbl.add functions "value" (Obj.repr value);
      Hashtbl.add functions "number?" (Obj.repr is_number);
      Hashtbl.add functions "+" (Obj.repr add);
      Hashtbl.add functions "-" (Obj.repr sub);
      Hashtbl.add functions "*" (Obj.repr mul);
      Hashtbl.add functions "/" (Obj.repr div);
      Hashtbl.add functions ">" (Obj.repr gt);
      Hashtbl.add functions "<" (Obj.repr lt);
      Hashtbl.add functions "<=" (Obj.repr gte);
      Hashtbl.add functions ">=" (Obj.repr lte);
      Hashtbl.add functions "string?" (Obj.repr is_string);
      Hashtbl.add functions "pos" (Obj.repr pos);
      Hashtbl.add functions "tlstr" (Obj.repr tlstr);
      Hashtbl.add functions "cn" (Obj.repr cn);
      Hashtbl.add functions "str" (Obj.repr str);
      Hashtbl.add functions "string->n" (Obj.repr string_to_n);
      Hashtbl.add functions "n<-string" (Obj.repr n_to_string);
      Hashtbl.add functions "absvector" (Obj.repr absvector);
      Hashtbl.add functions "cons?" (Obj.repr is_cons);
      Hashtbl.add functions "hd" (Obj.repr hd);
      Hashtbl.add functions "tl" (Obj.repr tl);
      Hashtbl.add functions "write-byte" (Obj.repr write_byte);
      Hashtbl.add functions "read-byte" (Obj.repr read_byte);
      Hashtbl.add functions "open" (Obj.repr open_stream);
      Hashtbl.add functions "close" (Obj.repr close_stream);
      Hashtbl.add functions "=" (Obj.repr eq);
      Hashtbl.add functions "get-time" (Obj.repr get_time);
      Hashtbl.add functions "boolean?" (Obj.repr is_boolean);
      Hashtbl.add functions "symbol?" (Obj.repr is_symbol);
      ()
  end
let _ =
  (Shen'1.write_byte (Obj.magic (Shen'1.Number 59.))
     (Obj.magic (Shen'1.value (Obj.magic (Shen'1.Symbol "*stoutput*")))))
    |> ignore;
  (Shen'1.read_byte
     (Obj.magic (Shen'1.value (Obj.magic (Shen'1.Symbol "*stinput*")))))
    |> ignore;
  ()