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

let load_source name =
    (*TODO*)
    ()

let main () =
    let filenames = ref [] in
    let do_test = ref false in
    Arg.parse [("-t", Arg.Unit (fun () -> do_test := true), "  test mode");]
        (fun name -> filenames := name::!filenames)
        "usage: tml [-t] filename...";
    List.iter load_source (List.rev !filenames);
    if !do_test then
        Test.test()
    else if List.length !filenames = 0 then
        top_level()
    else ()

let () = main ()

