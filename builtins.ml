
open Syntax

let error s = raise (Error ("Runtime error: " ^ s))
let type_error s = error ("type '" ^ s ^ "' required")

let fn_nl _ =
    print_newline ();
    flush stdout;
    VUnit

let fn_putn = function
    | VInt n -> print_int n; VUnit
    | _ -> type_error "int"

let fn_puts = function
    | VString s -> print_string s; VUnit
    | _ -> type_error "string"

let builtins =
    [
        ("nl", fn_nl);
        ("putn", fn_putn);
        ("puts", fn_puts);
    ]

let init () =
    let add_func (name, fn) =
        Eval.g_env := env_extend !Eval.g_env name (ref (VBuiltin fn))
    in
    List.iter add_func builtins
