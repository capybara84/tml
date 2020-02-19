open Syntax

let error msg = raise (Error ("Runtime Error: " ^ msg))

let g_env = ref []

let load_file filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let text = really_input_string ic n in
    close_in ic;
    text

let eval_unary = function
    | (UMinus, VInt n) -> VInt (-n)
    | (UNot, VBool b) -> VBool (not b)
    | _ -> error "type error (unary expression)"

let eval_binary = function
    | (BinAdd, VInt x, VInt y) -> VInt (x + y)
    | (BinAdd, VString x, VString y) -> VString (x ^ y)
    | (BinSub, VInt x, VInt y) -> VInt (x - y)
    | (BinMul, VInt x, VInt y) -> VInt (x * y)
    | (BinDiv, VInt x, VInt y) -> VInt (x / y)
    | (BinLT, VInt x, VInt y) -> VBool (x < y)
    | (BinLT, VChar x, VChar y) -> VBool (x < y)
    | (BinLT, VString x, VString y) -> VBool (x < y)
    | (BinLE, VInt x, VInt y) -> VBool (x <= y)
    | (BinLE, VChar x, VChar y) -> VBool (x <= y)
    | (BinLE, VString x, VString y) -> VBool (x <= y)
    | (BinGT, VInt x, VInt y) -> VBool (x > y)
    | (BinGT, VChar x, VChar y) -> VBool (x > y)
    | (BinGT, VString x, VString y) -> VBool (x > y)
    | (BinGE, VInt x, VInt y) -> VBool (x >= y)
    | (BinGE, VChar x, VChar y) -> VBool (x >= y)
    | (BinGE, VString x, VString y) -> VBool (x >= y)
    | (BinEql, VInt x, VInt y) -> VBool (x = y)
    | (BinEql, VBool x, VBool y) -> VBool (x = y)
    | (BinEql, VChar x, VChar y) -> VBool (x = y)
    | (BinEql, VString x, VString y) -> VBool (x = y)
    | (BinNeq, VInt x, VInt y) -> VBool (x <> y)
    | (BinNeq, VBool x, VBool y) -> VBool (x <> y)
    | (BinNeq, VChar x, VChar y) -> VBool (x <> y)
    | (BinNeq, VString x, VString y) -> VBool (x <> y)
    | _ -> error "type error (binary expression)"


let rec eval env = function
    | Eof -> VUnit
    | Unit -> VUnit
    | EInt n -> VInt n
    | EBool b -> VBool b
    | Ident x ->
        (try !(env_lookup env x) with Not_found -> error ("'" ^ x ^ "' not found"))
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
    | Fn (arg, body) ->
        VClosure (arg, body, env)
    | Apply (fn, arg) ->
        let closure = eval env fn in
        let arg_value = eval env arg in
        begin match closure with
            | VClosure (Ident carg, body, closure_env) ->
                let app_env = env_extend closure_env carg (ref arg_value) in
                eval app_env body
            | VClosure (Unit, body, closure_env) ->
                eval closure_env body
            | VBuiltin builtin ->
                builtin arg_value
            | v -> error ("application of non-function: " ^ value_to_str v)
        end
    | Let (id, e) ->
        let v = eval env e in
        g_env := env_extend !g_env id (ref v);
        VUnit
    | Letrec (id, fn) ->
        let r = ref VUnit in
        g_env := env_extend !g_env id r;
        r := eval !g_env fn;
        VUnit
    | LetIn (id, e, body) ->
        let v = eval env e in
        let env = env_extend env id (ref v) in
        eval env body
    | LetrecIn (id, fn, body) ->
        let r = ref VUnit in
        let renv = env_extend env id r in
        r := eval renv fn;
        eval renv body
    | Comp (e1, Unit) ->
        eval env e1
    | Comp (e1, e2) ->
        let v = eval env e1 in
        if v <> VUnit then error ("unit required") else ();
        eval env e2

(*
let rec print_env = function
    | [] -> ()
    | (id, v)::xs ->
        print_endline @@ " " ^ id ^ " = " ^ value_to_str !v;
        print_env xs
*)

let eval_line text =
    eval !g_env @@ Parser.parse_one @@ Scanner.from_string text

let eval_all el =
    let rec loop = function
        | [] -> ()
        | x::xs ->
            let v = eval !g_env x in
            if v <> VUnit then
                print_endline "Warning: The expression should have type unit"
            else ();
            loop xs
    in loop el

let load_source filename =
    try
        let text = load_file filename in
        eval_all @@ Parser.parse @@ Scanner.from_string text
    with
        | Error s | Sys_error s -> print_endline s
        | End_of_file -> ()

