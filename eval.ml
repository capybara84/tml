
type id = string

type exp = Eint of int | Ebool of bool | Symbol of id
    | Add of exp * exp |  Sub of exp * exp | Mul of exp * exp | Div of exp * exp
    | Eq of exp * exp | Lt of exp * exp | Le of exp * exp | And of exp * exp
    | Or of exp * exp | Not of exp | Minus of exp | If of exp * exp * exp
    | Let of id * exp * exp | Letrec of id * exp * exp | Fn of id * exp
    | Apply of exp * exp

type value = VUnit | VInt of int | VBool of bool
    | Closure of id * exp * env_t
    | RecClosure of id * id * exp * env_t
and env_t = (id * value ref) list

let env_extend env id value = (id, value) :: env
let env_lookup env id = List.assoc id env


let rec exp_to_str = function
    | Eint n -> "Eint " ^ string_of_int n
    | Ebool b -> "Ebool " ^ string_of_bool b
    | Symbol id -> "Symbol \"" ^ id ^ "\""
    | Add (x, y) -> "Add (" ^ exp_to_str x ^ ", " ^ exp_to_str y ^ ")"
    | Sub (x, y) -> "Sub (" ^ exp_to_str x ^ ", " ^ exp_to_str y ^ ")"
    | Mul (x, y) -> "Mul (" ^ exp_to_str x ^ ", " ^ exp_to_str y ^ ")"
    | Div (x, y) -> "Div (" ^ exp_to_str x ^ ", " ^ exp_to_str y ^ ")"
    | Eq (x, y) -> "Eq (" ^ exp_to_str x ^ ", " ^ exp_to_str y ^ ")"
    | Lt (x, y) -> "Lt (" ^ exp_to_str x ^ ", " ^ exp_to_str y ^ ")"
    | Le (x, y) -> "Le (" ^ exp_to_str x ^ ", " ^ exp_to_str y ^ ")"
    | And (x, y) -> "And (" ^ exp_to_str x ^ ", " ^ exp_to_str y ^ ")"
    | Or (x, y) -> "Or (" ^ exp_to_str x ^ ", " ^ exp_to_str y ^ ")"
    | Not e -> "Not " ^ exp_to_str e
    | Minus e -> "Minus " ^ exp_to_str e
    | If (c, t, e) -> "If (" ^ exp_to_str c ^ ", " ^ exp_to_str t ^ ", " ^ exp_to_str e ^ ")"
    | Let (id, e, b) -> "Let (\"" ^ id ^ "\", " ^ exp_to_str e ^ ", " ^ exp_to_str b ^ ")"
    | Letrec (id, e, b) -> "Letrec (\"" ^ id ^ "\", " ^ exp_to_str e ^ ", " ^ exp_to_str b ^ ")"
    | Fn (a, b) -> "Fn (\"" ^ a ^ "\", " ^ exp_to_str b ^ ")"
    | Apply (f, a) -> "Apply (" ^ exp_to_str f ^ ", " ^ exp_to_str a ^ ")"

let value_to_str = function
    | VUnit -> "()"
    | VInt n -> string_of_int n
    | VBool b -> string_of_bool b
    | Closure (p, b, _) -> "<fn " ^ p ^ " -> " ^ exp_to_str b ^ ">" 
    | RecClosure (f, p, b, _) -> "<" ^ f ^ " " ^ p ^ " -> " ^ exp_to_str b ^ ">" 

let int_add = function
    | VInt a, VInt b -> VInt (a + b)
    | _ -> failwith "int expected"

let int_sub = function
    | VInt a, VInt b -> VInt (a - b)
    | _ -> failwith "int expected"

let int_mul = function
    | VInt a, VInt b -> VInt (a * b)
    | _ -> failwith "int expected"

let int_div = function
    | VInt a, VInt b -> VInt (a / b)
    | _ -> failwith "int expected"

let equal = function
    | VInt a, VInt b -> VBool (a = b)
    | VBool a, VBool b -> VBool (a = b)
    | _ -> failwith "int/bool expected"

let less = function
    | VInt a, VInt b -> VBool (a < b)
    | _ -> failwith "int expected"

let lesseq = function
    | VInt a, VInt b -> VBool (a <= b)
    | _ -> failwith "int expected"

let bool_not = function
    | VBool b -> VBool (not b)
    | _ -> failwith "bool expected"

let int_minus = function
    | VInt n -> VInt (-n)
    | _ -> failwith "int expected"

let rec eval env e =
(*
    print_endline @@ "Expression: " ^ exp_to_str e;
    let evaluated =
*)
    match e with
    | Eint n -> VInt n
    | Ebool b -> VBool b
    | Symbol x -> !(env_lookup env x)
    | Add (x, y) -> int_add (eval env x, eval env y)
    | Sub (x, y) -> int_sub (eval env x, eval env y)
    | Mul (x, y) -> int_mul (eval env x, eval env y)
    | Div (x, y) -> int_div (eval env x, eval env y)
    | Eq (x, y) -> equal (eval env x, eval env y)
    | Lt (x, y) -> less (eval env x, eval env y)
    | Le (x, y) -> lesseq (eval env x, eval env y)
    | Not x -> bool_not (eval env x)
    | Minus x -> int_minus (eval env x)
    | And (x, y) ->
        if eval env x = VBool false then
            VBool false
        else
            eval env y
    | Or (x, y) ->
        if eval env x = VBool true then
            VBool true
        else
            eval env y
    | If (c, t, e) ->
        begin match eval env c with
            | VBool true -> eval env t
            | VBool false -> eval env e
            | _ -> failwith "non-boolean"
        end
    | Let (id, e, body) ->
        let v = eval env e in
        let env = env_extend env id (ref v) in
        eval env body
    | Letrec (id, fn, body) ->
        let r = ref VUnit in
        let renv = env_extend env id r in
        r := eval renv fn;
        eval renv body
    | Fn (arg, body) ->
        Closure (arg, body, env)
    | Apply (fn, arg) ->
        let closure = eval env fn in
        let arg_value = eval env arg in
        begin match closure with
            | Closure (carg, body, closure_env) ->
                let app_env = env_extend closure_env carg (ref arg_value) in
                eval app_env body
            | v -> failwith ("application of non-function: " ^ value_to_str v)
        end
(*
    in
    print_endline @@ "Evaluated: " ^ value_to_str evaluated;
    evaluated
*)

(*
let env0 = [];;

let e1 = Apply (Fn ("y", Add(Symbol "y", Eint 1)), Eint 3);;

print_endline @@ exp_to_str e1;;
print_endline @@ value_to_str @@ eval env0 e1;;

let e2 = Apply (Let ("x", Eint 2, Fn ("y", Add (Symbol "y", Symbol "x"))), Eint 3);;

print_endline @@ exp_to_str e2;;
print_endline @@ value_to_str @@ eval env0 e2;;

let e3 = Letrec ("fact",
            (Fn ("x",
                (If (Le (Symbol "x", Eint 0), Eint 1,
                    Mul (Symbol "x",
                        Apply (Symbol "fact", Sub (Symbol "x", Eint 1))))))),
            Apply (Symbol "fact", Eint 5));;

print_endline @@ exp_to_str e3;;
print_endline @@ value_to_str @@ eval env0 e3;;
*)
