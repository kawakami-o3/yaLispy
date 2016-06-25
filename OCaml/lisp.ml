#load "str.cma"

let join = String.concat ","
let zip a b = List.map2 (fun x y -> x,y) a b
let sum lst = List.fold_left (fun s i -> s + i) 0 lst
let rec all f arr =
  match arr with
  | [] -> true
  | x :: xs -> if (f x) then all f xs else false
 

type sexp =
      | LispInt of int
      | LispSymbol of string
      | LispProc of (sexp list -> sexp)
      | LispLambda of (sexp list)
      | LispList of (sexp list)

type environment = Environment of (((string * sexp) list) * (environment option))

let lisp_nil = LispSymbol "nil"
let lisp_true = LispSymbol "t"
let lisp_false = LispSymbol "f"
let lisp_error = LispSymbol "error"

let lisp_bool cond = if cond then lisp_true else lisp_false


let atom s =
  try LispInt (int_of_string s)
  with _ -> LispSymbol s

let rec to_sym sexp =
  match sexp with
  | LispSymbol s -> s
  | a -> to_sym lisp_error

let to_list sexp =
  match sexp with
  | LispList lst -> lst
  | a -> [lisp_nil]

let to_int sexp =
  match sexp with
  | LispInt i -> i
  | a -> 0

let rec to_string sexp =
  match sexp with
  | LispInt i -> "Int(" ^ (string_of_int i) ^ ")"
  | LispSymbol s -> "Sym(\"" ^ s ^ "\")"
  | LispProc lst -> "Proc[]"
  | LispList lst -> "List[" ^ (join (List.map to_string lst)) ^ "]"
  | LispLambda lst -> "Lambda[]"

let error_few_args = atom "too few arguments"
let error_many_args = atom "too many arguments"
let error_undefined = atom "undefined"

let rec proc_append lst =
  match lst with
  | [] -> LispList []
  | x::xs -> LispList (List.append (to_list x) (to_list (proc_append xs)))

let proc_car lst =
  match lst with
  | [] -> error_few_args
  | x :: xs -> List.hd (to_list x)

let proc_cdr lst =
  match lst with
  | [] -> error_few_args
  | x::xs -> LispList (List.tl (to_list x))

let proc_cons lst =
  match lst with
  | [] -> error_few_args
  | a::(LispList es)::[] -> LispList (a::es)
  | a::b::[] -> LispList [a;b]
  | _ -> error_many_args

let proc_length lst =
  match lst with
  | (LispList es)::[] -> LispInt (List.length es)
  | _ -> error_undefined

let rec proc_list lst =
  match lst with
  | [] -> LispList []
  | x::xs -> LispList (x :: (to_list (proc_list xs)))

let proc_is_list lst =
  match lst with
  | (LispList x)::xs -> lisp_true
  | _ -> lisp_false

let proc_is_symbol lst =
  match lst with
  | (LispSymbol x)::xs -> lisp_true
  | _ -> lisp_false

let proc_is_nil lst = lisp_bool ((List.hd lst) == lisp_nil)
let proc_not lst = lisp_bool ((List.hd lst) == lisp_true)

let proc_plus lst = LispInt (sum (List.map to_int lst))

let proc_minus lst =
  match lst with
  | (LispInt x)::[] -> LispInt (- x)
  | (LispInt x)::xs -> LispInt (x - (sum (List.map to_int xs)))
  | _ -> error_few_args

let rec proc_multiply lst =
  match lst with
  | [] -> LispInt 1
  | (LispInt 0) :: xs -> LispInt 0
  | (LispInt x) :: xs -> LispInt (x * (to_int (proc_multiply xs)))
  | _ -> error_undefined

let proc_divide lst =
  match lst with
  | [] -> LispInt 1
  | (LispInt 0)::xs -> LispInt 0
  | (LispInt x)::xs -> LispInt (x / (to_int (proc_multiply xs)))
  | _ -> error_undefined


let compare_all f lst =
   match lst with
  | (LispInt x)::xs -> lisp_bool (all (fun i -> (f x i)) (List.map to_int xs))
  | _ -> error_undefined

let proc_greater = compare_all (fun x i -> x > i)
let proc_less = compare_all (fun x i -> x < i)
let proc_greater_equal = compare_all (fun x i -> x >= i)
let proc_less_equal = compare_all (fun x i -> x <= i)
let proc_equal = compare_all (fun x i -> x == i)

let global_environment =
    Environment ([
      ("append", LispProc proc_append);
      ("car", LispProc proc_car);
      ("cdr", LispProc proc_cdr);
      ("cons", LispProc proc_cons);
      ("length", LispProc proc_length);
      ("list", LispProc proc_list);

      ("list?", LispProc proc_is_list);
      ("symbol?", LispProc proc_is_symbol);
      ("nil?", LispProc proc_is_nil);
      ("not", LispProc proc_not);
      
      ("+", LispProc proc_plus);
      ("-", LispProc proc_minus);
      ("*", LispProc proc_multiply);
      ("/", LispProc proc_divide);

      (">", LispProc proc_greater);
      ("<", LispProc proc_less);
      (">=", LispProc proc_greater_equal);
      ("<=", LispProc proc_less_equal);
      ("=", LispProc proc_equal);

      ("t",lisp_true);
      ("f",lisp_false);
      ("nil",lisp_nil)
    ], None)


let rec search_dict lst sym =
  match lst with
  | [] -> None
  | (s,e)::rest ->
      if s = sym then
        Some e
      else
        search_dict rest sym

let rec search_env env sym =
  match env with
  | None -> (None, lisp_nil)
  | Some Environment (lst, parent) ->
      match (search_dict lst sym) with
      | None -> search_env parent sym
      | Some exp -> (Some (Environment (lst, parent)), exp)

let define_dict env sym sexp =
  let Environment (lst, parent) = env
  in Environment (((sym,sexp)::lst), parent)

let set_dict env sym sexp =
  match search_env (Some env) sym with
  | (None, a) -> define_dict env sym sexp
  | (Some e, a) -> define_dict e sym sexp



let rec eval env sexp =
  let
    eval_proc env sexp =
      match List.map (fun x -> snd (eval env x)) (to_list sexp) with
      | [] -> (env, lisp_error)
      | (proc::exps) -> 
          match proc with
          | LispLambda (l::args::body::a) ->
              let syms = List.map to_sym (to_list args)
              in eval (Environment ((zip syms exps), Some env)) body
          | LispProc p -> (env, p exps)
          | a -> (env, atom "undefined eval")
  in
  let
    eval_list_quote lst = List.hd lst
  in
  let
    eval_list_if lst =
      match lst with
      | (cond::a::b::_) -> if lisp_true == (snd (eval env cond)) then eval env a else eval env a
      | _ -> (env, lisp_nil)
  in
  let
    eval_list_set lst =
      match lst with
      | ((LispSymbol key)::cell::_) -> let c = snd (eval env cell) in ((set_dict env key c), c)
      | _ -> (env, lisp_nil)
  in
  let
    eval_list_define lst =
      match lst with
      | ((LispSymbol key)::cell::_) -> let c = snd (eval env cell) in ((define_dict env key c), c)
      | _ -> (env, lisp_nil)
  in
  let
    eval_list env sexp =
      let lst = to_list sexp in
      let rest = List.tl lst in
      match to_sym (List.hd lst) with
      | "quote" -> (env, eval_list_quote rest)
      | "if" -> eval_list_if rest
      | "set!" -> eval_list_set rest
      | "define" -> eval_list_define rest
      | "lambda" -> (env, LispLambda (to_list sexp))
      | _ -> eval_proc env sexp
  in
  match sexp with
  | LispSymbol sym -> (env, snd (search_env (Some env) sym))
  | LispInt _ -> (env, sexp)
  | LispList [] -> (env, lisp_nil)
  | LispList _ -> eval_list env sexp
  | _ -> eval_proc env sexp

let rec read_from str_list =
  let
    rec read_list exprs lst =
      match lst with
      | [] -> [lisp_nil], []
      | ")"::xs -> (List.rev exprs), xs
      | x::xs -> let (expr, ss) = read_from (x::xs) in read_list (expr::exprs) ss
  in
    match str_list with
    | [] -> (lisp_nil, [])
    | ("("::xs) -> let (lst, ss) = read_list [] xs in (LispList lst, ss)
    | (")"::xs) -> (lisp_nil, [])
    | (x::xs) -> (atom x, xs)


let tokenize s =
  let add_space = Str.global_replace (Str.regexp "[()]") " \\0 "
  in Str.split (Str.regexp " +") (String.trim (add_space s))

let parse s = fst (read_from (tokenize s))

let rec repl env = (

  print_string "MyLisp> ";
  flush stdout;
  let
    s = input_line stdin
  in
  let
    (e,sexp) = eval env (parse s)
  in
    print_string "-> ";
    print_string (to_string sexp);
    print_string "\n";

    flush stdout;
    repl e;
)

let _ = repl global_environment
