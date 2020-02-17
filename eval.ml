
type id = string

type exp = Eint of int | Ebool of bool | Symbol of id
    | Add of exp * exp |  Sub of exp * exp | Mul of exp * exp | Div of exp * exp
    | Eq of exp * exp | Lt of exp * exp | Le of exp * exp | And of exp * exp
    | Or of exp * exp | Not of exp | Minus of exp | If of exp * exp * exp
    | Let of id * exp * exp | Letrec of id * exp * exp | Fn of exp * exp
    | Apply of exp * exp

type 't env_t = (id * 't) list

type value = VInt of int | VBool of bool
    | Closure of id * exp * value env_t
    | RecClosure of id * id * exp * value env_t


let env_extend env id v = env (*TODO*)
let lookup env id = VInt 0 (*TODO*)

let rec exp_to_str = function
    | Eint n -> "Eint " ^ string_of_int n
    | Ebool b -> "Ebool " ^ string_of_bool b
    | Symbol id -> "Symbol " ^ id
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
    | Let (id, e, b) -> "Let (" ^ id ^ ", " ^ exp_to_str e ^ ", " ^ exp_to_str b ^ ")"
    | Letrec (id, e, b) -> "Letrec (" ^ id ^ ", " ^ exp_to_str e ^ ", " ^ exp_to_str b ^ ")"
    | Fn (p, b) -> "Fn (" ^ exp_to_str p ^ ", " ^ exp_to_str b ^ ")"
    | Apply (f, a) -> "Apply (" ^ exp_to_str f ^ ", " ^ exp_to_str a ^ ")"

let rec value_to_str = function
    | VInt n -> string_of_int n
    | VBool b -> string_of_bool b
    | Closure (p, b, _) -> "<fn " ^ p ^ " -> " ^ exp_to_str b ^ ">" 
    | RecClosure (f, p, b, _) -> "<" ^ f ^ " " ^ p ^ " -> " ^ exp_to_str b ^ ">" 


let rec eval (env : value env_t) = function
    | Eint n -> VInt n
    | Ebool b -> VBool b
    | Symbol x -> lookup env x
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
    | Let (id, e, b) ->
        eval (env_extend env id (eval env e)) b
    | Letrec (id, v, body) ->
        begin match v with
            | Fn (param, fbody) ->
                let renv = env_extend env id (RecClosure (id, param, fbody, env))
                in eval renv body
            | _ -> failwith "non-function"
        end
    | Fn (p, b) ->
        Closure (p, b, env)
    | Apply (f, param) ->
        begin match eval env f with
            | Closure (arg, body, closure_env) ->
                let vparam = eval env param in
                let app_env = env_extend closure_env arg vparam in
                eval app_env body
            | RecClosure (name, arg, body, closure_env) ->
                let vparam = eval env param in
                let app_env = env_extend closure_env arg vparam in
                eval app_env body
        end


