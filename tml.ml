open Syntax

let rec top_level () =
    try
        print_string "> ";
        flush stdout;
        let line = input_line stdin in
        let v = Eval.eval_line line in
        print_endline @@ value_to_str v;
        top_level ()
    with
        | Error s -> (print_endline s; top_level ())
        | Sys_error s -> print_endline s
        | End_of_file -> ()

let main () =
    let filenames = ref [] in
    let do_test = ref false in
    Arg.parse [ ("-d", Arg.Int (fun x -> Parser.debug := x),   "N   parser debug level N");
                ("-t", Arg.Unit (fun () -> do_test := true),   "    test mode");
                ("-v", Arg.Unit (fun () -> g_verbose := true), "    verbose mode");
              ]
        (fun name -> filenames := name::!filenames)
        "usage: tml [-t] filename...";
    Builtins.init ();
    List.iter Eval.load_source (List.rev !filenames);
    if !do_test then
        Test.test ()
    else if List.length !filenames = 0 then
        top_level ()
    else ()

let () = main ()

