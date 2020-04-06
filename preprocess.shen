(define preproc
  Env [defun F [] Body] -> (preproc Env [defun F [(gensym x)] Body])
  Env [defun F [Param | Params] Body] ->
    [defun F [Param]
      (lambda-form Params
      (quote-free-symbols (append [Param | Params] Env) (curry (macros Body))))]
  Env X -> (quote-free-symbols Env (curry (macros X))))

(define macros
  [and X Y] -> [if (macros X) (macros Y) false]
  [or X Y] -> [if (macros X) true (macros Y)]
  [let X Y Z] -> [[lambda (macros X) (macros Z)] (macros Y)]
  [freeze X] -> [lambda (gensym x) (macros X)]
  [cond [true X]] -> (macros X)
  [cond] -> [simple-error "case failure"]
  [cond [P Q] | R] -> (macros [if (macros P) (macros Q) [cond | (macros R)]])
  [do X] -> [(macros X)]
  [do X | Z] -> (macros [let (gensym x) (macros X) | (macros [do | Z])])
  [X | Y] -> [(macros X) | (map (function macros) Y)]
  X -> X)

(define curry
  [defun F Params Body] -> [defun F Params (curry Body)]
  [R X Y | Z] -> (curry [[R X] Y | Z])
  [X Y] -> [(curry X) (curry Y)]
  X -> X)

(define quote-free-symbols
  Bound [defun F [Param | Params] Body] ->
    [defun F [Param | Params]
      (quote-free-symbols [F | (append [Param | Params] Bound)] Body)]
  Bound [lambda X Y] -> [lambda X (quote-free-symbols [X | Bound] Y)]
  Bound [R | X] -> [R | (map (/. Y (quote-free-symbols Bound Y)) X)] where (symbol? R)
  Bound [X | Y] ->
    [(quote-free-symbols Bound X)
      | (map (/. Z (quote-free-symbols Bound Z)) Y)]
  Bound X -> [quote X] where (and (symbol? X) (not (element? X Bound)))
  _ X -> X)

(define lambda-form
  [] Body -> Body
  [Param | Params] Body -> [lambda Param (lambda-form Params Body)])