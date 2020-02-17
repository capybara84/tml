open Syntax

let env = []

let rec top_level () =
    try
        print_string "> ";
        flush stdout;
        let line = input_line stdin in
        let v = Eval.eval env @@ Parser.parse_one @@ Scanner.from_string line in
        print_endline @@ value_to_str v;
        top_level ()
    with
        | Error s -> (print_endline s; top_level ())
        | Sys_error s -> print_endline s
        | End_of_file -> ()

let () =
    Test.test();
    top_level()

