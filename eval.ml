open Syntax

let error msg =
    raise (Error ("Runtime Error: " ^ msg))

let eval_unary = function
    | (UMinus, VInt n) -> VInt (-n)
    | (UNot, VBool b) -> VBool (not b)
    | _ -> error "type error (unary expression)"

let eval_binary = function
    | (BinAdd, VInt x, VInt y) -> VInt (x + y)
    | (BinSub, VInt x, VInt y) -> VInt (x - y)
    | (BinMul, VInt x, VInt y) -> VInt (x * y)
    | (BinDiv, VInt x, VInt y) -> VInt (x / y)
    | (BinLT, VInt x, VInt y) -> VBool (x < y)
    | (BinLE, VInt x, VInt y) -> VBool (x <= y)
    | (BinGT, VInt x, VInt y) -> VBool (x > y)
    | (BinGE, VInt x, VInt y) -> VBool (x >= y)
    | (BinEql, VInt x, VInt y) -> VBool (x = y)
    | (BinEql, VBool x, VBool y) -> VBool (x = y)
    | (BinNeq, VInt x, VInt y) -> VBool (x <> y)
    | (BinNeq, VBool x, VBool y) -> VBool (x <> y)
    | _ -> error "type error (binary expression)"

let rec eval env = function
    | Eof -> VUnit
    | Unit -> VUnit
    | EInt n -> VInt n
    | EBool b -> VBool b
    | Ident x -> !(env_lookup env x)
    | EChar c -> VChar c
    | EString s -> VString s
    | Binary (BinLand, x, y) ->
        if eval env x = VBool false then
            VBool false
        else
            eval env y
    | Binary (BinLor, x, y) ->
        if eval env x = VBool true then
            VBool true
        else
            eval env y
    | Binary (op, x, y) -> eval_binary (op, eval env x, eval env y)
    | Unary (op, e) -> eval_unary (op, eval env e)
    | If (c, t, e) ->
        begin match eval env c with
            | VBool true -> eval env t
            | VBool false -> eval env e
            | _ -> error "non-boolean"
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
            | Closure (Ident carg, body, closure_env) ->
                let app_env = env_extend closure_env carg (ref arg_value) in
                eval app_env body
            | v -> error ("application of non-function: " ^ value_to_str v)
        end
