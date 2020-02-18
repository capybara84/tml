
open Syntax

let color c s = "\x1b[3" ^ (string_of_int c) ^ "m" ^ s ^ "\x1b[0m"
let red = color 1
let green = color 2
let yellow = color 3
let blue = color 4
let magenta = color 5
let cyan = color 6
let white = color 7

let n_ok = ref 0
let n_fail = ref 0

let ok () =
    incr n_ok;
    print_string @@ green "."

let fail s =
    incr n_fail;
    print_endline @@ red "!" ^ s

let equal a b m =
    if a = b then
        ok ()
    else
        fail m

let report () =
    let n_all = !n_ok + !n_fail in
    print_endline ("All   : " ^ (string_of_int n_all));
    print_endline ("OK    : " ^ (green @@ string_of_int !n_ok));
    print_endline ("Failed: " ^ (if !n_fail = 0 then "0" else (red @@ string_of_int !n_fail)))

(*
let simple_test () =
    let env0 = [] in
    let e1 = Apply (Fn (Ident "y", Binary (BinAdd, Ident "y", EInt 1)), EInt 3) in
    let e2 = Apply (Let ("x", EInt 2,
                        Fn (Ident "y", Binary (BinAdd, Ident "y", Ident "x"))), EInt 3) in
    let e3 = Letrec ("fact",
                (Fn (Ident "x",
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
*)

let test_text = "
identifier Ident 12345
'a' '\\t' \"abc\\n\"
let rec in fn if then else
= == != < <= > >=
- + / *
! | || &&
-> ( ) , ;
"
let test_tokens = [
    ID "identifier"; ID "Ident"; INT_LIT 12345;
    CHAR_LIT 'a'; CHAR_LIT '\t'; STRING_LIT "abc\n";
    LET; REC; IN; FN; IF; THEN; ELSE;
    EQ; EQL; NEQ; LT; LE; GT; GE;
    MINUS; PLUS; SLASH; STAR;
    NOT; OR; LOR; LAND;
    ARROW; LPAR; RPAR; COMMA; SEMI;
    EOF
]

let scanner_test verbose =
    print_string "Scanner Test:";
    let tokens = Scanner.from_string test_text in
    let len_tt = List.length test_tokens in
    let len_t = List.length tokens in
    equal len_tt len_t ("length " ^ string_of_int len_tt
                                    ^ " != " ^ string_of_int len_t);
    if verbose then
        List.iter
            (fun x ->
                print_endline ("[" ^ token_type_to_string x.token_type ^ ", "
                    ^ string_of_int x.line ^ "]")) tokens
    else ();
    List.iter2 (fun tt t -> equal tt t.token_type
                ((token_type_to_string tt) ^ " != " ^ (token_to_string t)))
            test_tokens tokens;
    print_newline ()

let parse_test_exprs = [
    ("'a'", EChar 'a');
    ("\"abc\"", EString "abc");
    ("12", EInt 12);
    ("300 + 12",
        Binary (BinAdd, EInt 300, EInt 12));
    ("300 * 12 + 3",
        Binary (BinAdd, Binary (BinMul, EInt 300, EInt 12), EInt 3));
    ("300 * (12 + 3)",
        Binary (BinMul, EInt 300,
                Binary (BinAdd, EInt 12, EInt 3)));
    ("1 / 2 < 3 * 4",
        Binary (BinLT, (Binary (BinDiv, EInt 1, EInt 2)),
                        (Binary (BinMul, EInt 3, EInt 4))));
    ("2 * -(1 + 2)",
        Binary (BinMul, EInt 2,
            (Unary (UMinus, Binary (BinAdd, EInt 1, EInt 2)))));
    ("a && b",
        Binary (BinLand, Ident "a", Ident "b"));
    ("a || b",
        Binary (BinLor, Ident "a", Ident "b"));
    ("!(x < y)",
        Unary (UNot, Binary (BinLT, Ident "x", Ident "y")));
    ("1 <= 2",
        Binary (BinLE, EInt 1, EInt 2));
    ("1 > 2",
        Binary (BinGT, EInt 1, EInt 2));
    ("1 >= 2",
        Binary (BinGE, EInt 1, EInt 2));
    ("1 == 2",
        Binary (BinEql, EInt 1, EInt 2));
    ("1 != 2",
        Binary (BinNeq, EInt 1, EInt 2));
    ("fn x -> x + 1",
        Fn (Ident "x", Binary (BinAdd, Ident "x", EInt 1)));
    ("f 3",
        Apply (Ident "f", EInt 3));
    ("-(f 3)",
        Unary (UMinus, Apply (Ident "f", EInt 3)));
    ("f (-3)",
        Apply (Ident "f", Unary (UMinus, EInt 3)));
    ("f -3",
        Binary (BinSub, Ident "f", EInt 3));
    ("fn () -> 1",
        Fn (Unit, EInt 1));
    ("(fn x -> x + 1) (300 * (12 + 3))",
        Apply (Fn (Ident "x", Binary (BinAdd, Ident "x", EInt 1)), 
            Binary (BinMul, EInt 300,
                Binary (BinAdd, EInt 12, EInt 3))));
    ("let fact = fn n -> if n < 1 then 1 else n * fact (n - 1) in fact 5",
            Let ("fact",
                Fn (Ident "n",
                    (If (Binary (BinLT, Ident "n", EInt 1),
                          EInt 1,
                          Binary (BinMul, Ident "n",
                                        Apply (Ident "fact",
                                            Binary (BinSub, Ident "n",
                                                EInt 1)))))),
                Apply (Ident "fact", EInt 5)));
    ("1+2+3",
        (Binary (BinAdd,
            (Binary (BinAdd, EInt 1, EInt 2)), EInt 3)));
    ("f 1 2",
        Apply (Apply (Ident "f", EInt 1), EInt 2));
    ("f 1 2 3",
        (Apply (Apply (Apply (Ident "f",
            EInt 1), EInt 2), EInt 3)));
    ("(1)", EInt 1);
    ("true", EBool true);
    ("false", EBool false);
    ("(1)", EInt 1);
]

let parser_test verbose =
    print_string "Parser Test:";
    let do_parse (text, expected) =
        try
            if verbose then 
                print_endline ("text    > " ^ text)
            else ();
            let expr = Parser.parse_one @@ Scanner.from_string text in
            let parsed = exp_to_str expr in
            let expected = exp_to_str expected in
            if verbose then begin
                print_endline ("parsed  > " ^ parsed);
                print_endline ("expected> " ^ expected)
            end else ();
            equal parsed expected ("text:" ^ text ^ "\nresult:" ^ parsed ^ " != " ^ expected ^ "\n")
        with Error s -> fail s
    in
    List.iter do_parse parse_test_exprs;
    print_newline ()

let test () =
(*
    simple_test ();
*)
    scanner_test false;
    parser_test false;
    report ()

