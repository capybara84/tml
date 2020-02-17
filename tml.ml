open Eval

let env0 = [];;
let e3 = Letrec ("fact",
            (Fn ("x",
                (If (Le (Symbol "x", Eint 0), Eint 1,
                    Mul (Symbol "x",
                        Apply (Symbol "fact", Sub (Symbol "x", Eint 1))))))),
            Apply (Symbol "fact", Eint 5));;

print_endline @@ exp_to_str e3;;
print_endline @@ value_to_str @@ eval env0 e3;;
