
open Syntax

let simple_test () =
    let env0 = [] in
    let e1 = Apply (Fn ("y", Binary (BinAdd, Ident "y", EInt 1)), EInt 3) in
    let e2 = Apply (Let ("x", EInt 2, Fn ("y", Binary (BinAdd, Ident "y", Ident "x"))), EInt 3) in
    let e3 = Letrec ("fact",
                (Fn ("x",
                    (If (Binary (BinLE, Ident "x", EInt 0), EInt 1,
                        Binary (BinMul, Ident "x",
                            Apply (Ident "fact", Binary (BinSub, Ident "x", EInt 1))))))),
                Apply (Ident "fact", EInt 5)) in

    print_endline @@ exp_to_str e1;
    print_endline @@ value_to_str @@ Eval.eval env0 e1;

    print_endline @@ exp_to_str e2;
    print_endline @@ value_to_str @@ Eval.eval env0 e2;

    print_endline @@ exp_to_str e3;
    print_endline @@ value_to_str @@ Eval.eval env0 e3

let test_text = "
identifier Ident 12345
'a' '\\t' \"abc\\n\"
module import as mutable
type unit bool int char float string
let fn if then else match
= == != < <= > >=
- + / *
! | || &&
-> ( ) , ;
"

let scanner_test () =
    let tokens = Scanner.from_string test_text in
    List.iter
        (fun x ->
            print_endline ("[" ^ token_type_to_string x.token_type ^ ", "
                ^ string_of_int x.line ^ "]")) tokens

let all_exprs = [
    "'a'";
    "\"abc\"";
    "12";
    "300 + 12";
    "300 * 12 + 3";
    "300 * (12 + 3)";
    "1 / 2 < 3 * 4";
    "2 * -(1 + 2)";
    "a && b";
    "a || b";
    "!(x < y)";
    "1 <= 2";
    "1 > 2";
    "1 >= 2";
    "1 == 2";
    "1 != 2";
    "fn x -> x + 1";
    "f 3";
    "-(f 3)";
    "f (-3)";
    "f -3";
    "fn _ -> 1";
    "(fn x -> x + 1) (300 * (12 + 3))";
    "let fact = fn n -> if n < 1 then 1 else n * fact (n - 1) in fact 5";
    "f 1 2";
    "f 1 2 3";
    "(1)";
    "true";
    "false";
]

let parser_test () =
    let do_parse text =
        try
            print_endline ("text > " ^ text);
            let e = Parser.parse_one @@ Scanner.from_string text in
            print_endline ("parsed> " ^ exp_to_str e)
        with Error s -> print_endline s
    in
    List.iter do_parse all_exprs

let test () =
    simple_test ();
    scanner_test ();
    parser_test ()

