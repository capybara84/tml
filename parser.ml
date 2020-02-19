
open Syntax

type t = {
    mutable tokens : token list;
}

let debug = ref 0
let debug_indent = ref 0
let rec debug_show_space n =
    if n = 0 then ()
    else begin
        print_char ' ';
        debug_show_space (n-1)
    end
let debug_print_indent level s =
    if !debug < level then ()
    else begin
        debug_show_space !debug_indent;
        print_endline s
    end
let debug_print_in level s =
    if !debug < level then ()
    else begin
        debug_show_space !debug_indent;
        incr debug_indent;
        print_endline ("IN " ^ s)
    end
let debug_print_out level s =
    if !debug < level then ()
    else begin
        decr debug_indent;
        debug_show_space !debug_indent;
        print_endline ("OUT " ^ s)
    end
let debug_print level s =
    if !debug < level then ()
    else
        print_endline s

let debug_parse_in msg = debug_print_in 3 msg
let debug_parse_out msg = debug_print_out 3 msg
let debug_parse msg = debug_print_indent 3 msg
let debug_token msg = debug_print 2 msg


let error _ msg =
    raise (Error msg)

let token_to_binop = function
    | PLUS -> BinAdd
    | MINUS -> BinSub
    | STAR -> BinMul
    | SLASH -> BinDiv
    | LT -> BinLT
    | LE -> BinLE
    | GT -> BinGT
    | GE -> BinGE
    | EQL -> BinEql
    | NEQ -> BinNeq
    | LOR -> BinLor
    | LAND -> BinLand
    | _ -> failwith "binop bug"

let token_to_unop = function
    | NOT -> UNot
    | MINUS -> UMinus
    | _ -> failwith "unop bug"

let is_unop = function
    | NOT | MINUS -> true | _ -> false

let is_mul_op = function
    | STAR | SLASH -> true | _ -> false

let is_add_op = function
    | PLUS | MINUS -> true | _ -> false

let is_equal_op = function
    | LT | LE | GT | GE | EQL | NEQ -> true | _ -> false

let is_logical_op = function
    | LOR | LAND -> true | _ -> false

let is_apply e t =
    match e with
    | Fn _ | Apply _ | Ident _ ->
        begin
            match t with
            | EMPTY | ID _ | BOOL_LIT _ | INT_LIT _
            | CHAR_LIT _ | STRING_LIT _ | LPAR -> true
            | _ -> false
        end
    | _ -> false

let peek_token pars =
    match pars.tokens with
    | [] -> {token_type=EOF;line=0}
    | tok::_ -> tok

let peek_token_type pars =
    let token = peek_token pars in
    token.token_type

let next_token pars =
    begin
        match pars.tokens with
        | [] -> ()
        | _::toks -> pars.tokens <- toks
    end;
    debug_token ("token = " ^ (token_to_string @@ peek_token pars))

let expect pars token_type =
    let current_type = peek_token_type pars in
    if current_type = token_type then
        next_token pars
    else
        error pars ("missing token '" ^ token_type_to_string token_type ^
                "' at '" ^ token_type_to_string current_type ^ "'")

let expect_id pars =
    match peek_token_type pars with
    | ID id -> next_token pars; id
    | t -> error pars ("missing identifier at '" ^ token_type_to_string t ^ "'")

let rec parse_simple pars =
    debug_parse_in "parse_simple";
    let res =
        match peek_token_type pars with
        | EOF -> Eof
        | EMPTY ->
            next_token pars;
            Unit
        | ID id ->
            next_token pars;
            Ident id
        | BOOL_LIT b ->
            next_token pars;
            EBool b
        | INT_LIT i ->
            next_token pars;
            EInt i
        | CHAR_LIT c ->
            next_token pars;
            EChar c
        | STRING_LIT s ->
            next_token pars;
            EString s
        | LPAR ->
            next_token pars;
            let e = parse_expr pars in
            expect pars RPAR;
            e
        | t ->
            next_token pars;
            error pars ("syntax error at '" ^ token_type_to_string t ^ "'")
    in
    debug_parse_out "parse_simple";
    res

and parse_unary pars =
    debug_parse_in "parse_unary";
    let op = peek_token_type pars in
    let res =
    if is_unop op then begin
        next_token pars;
        let e = parse_simple pars in
        Unary (token_to_unop op, e)
    end else
        parse_simple pars
    in
    debug_parse_out "parse_unary";
    res

and parse_apply pars =
    debug_parse_in "parse_apply";
    let rec parse_apply_rhs lhs =
        let a = parse_simple pars in
        let e = Apply (lhs, a) in
        if is_apply e (peek_token_type pars) then
            parse_apply_rhs e
        else
            e
    in
    let e = parse_unary pars in
    let res = if is_apply e (peek_token_type pars) then
        parse_apply_rhs e
    else
        e
    in
    debug_parse_out "parse_apply";
    res

and parse_mul pars =
    debug_parse_in "parse_mul";
    let rec parse_rhs lhs =
        let tt = peek_token_type pars in
        if not (is_mul_op tt) then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_apply pars in
            parse_rhs (Binary (op, lhs, rhs))
        end
    in
    let e = parse_apply pars in
    let e = parse_rhs e
    in
    debug_parse_out "parse_mul";
    e

and parse_add pars =
    debug_parse_in "parse_add";
    let rec parse_rhs lhs =
        let tt = peek_token_type pars in
        if not (is_add_op tt) then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_mul pars in
            parse_rhs (Binary (op, lhs, rhs))
        end
    in
    let e = parse_mul pars in
    let e = parse_rhs e
    in
    debug_parse_out "parse_add";
    e

and parse_cons pars =
    debug_parse_in "parse_cons";
    let e = parse_add pars in
    debug_parse_out "parse_cons";
    e

and parse_equal pars =
    debug_parse_in "parse_equal";
    let lhs = parse_cons pars in
    let tt = peek_token_type pars in
    let e =
    if not (is_equal_op tt) then
        lhs
    else begin
        let op = token_to_binop tt in
        next_token pars;
        let rhs = parse_cons pars in
        Binary (op, lhs, rhs)
    end
    in
    debug_parse_out "parse_equal";
    e

and parse_logical pars =
    debug_parse_in "parse_logical";
    let rec parse_rhs lhs =
        let tt = peek_token_type pars in
        if not (is_logical_op tt) then
            lhs
        else begin
            let op = token_to_binop tt in
            next_token pars;
            let rhs = parse_equal pars in
            parse_rhs (Binary (op, lhs, rhs))
        end
    in
    let e = parse_equal pars in
    let e = parse_rhs e in
    debug_parse_out "parse_logical";
    e

and parse_comp pars =
    debug_parse_in "parse_comp";
    next_token pars;
    let rec parse_rest pars =
        if peek_token_type pars <> SEMI then
            Unit
        else begin
            next_token pars;
            let e = parse_expr pars in
            Comp (e, parse_rest pars)
        end
    in
    let e1 = parse_expr pars in
    let e = Comp (e1, parse_rest pars) in
    expect pars END;
    debug_parse_out "parse_comp";
    e

and parse_if pars =
    debug_parse_in "parse_if";
    next_token pars;
    let e1 = parse_expr pars in
    expect pars THEN;
    let e2 = parse_expr pars in
    expect pars ELSE;
    let e3 = parse_expr pars in
    debug_parse_out "parse_if";
    If (e1, e2, e3)

and parse_param_list pars args =
    debug_parse_in "parse_param_list";
    let e =
        match peek_token_type pars with
        | ID id ->
            next_token pars;
            parse_param_list pars (Ident id::args)
        | _ ->
            List.rev args
    in
    debug_parse_out "parse_param_list";
    e

and parse_params pars =
    debug_parse_in "parse_params pars";
    let e =
        if peek_token_type pars = EMPTY then
            (next_token pars; [Unit])
        else
            parse_param_list pars []
    in
    debug_parse_out "parse_params pars";
    e

and parse_fn pars =
    debug_parse_in "parse_fn";
    next_token pars;
    let args = parse_params pars in
    expect pars ARROW;
    let e = List.fold_right (fun arg body -> Fn (arg, body)) args (parse_expr pars) in
    debug_parse_out "parse_fn";
    e

and parse_let pars =
    debug_parse_in "parse_let";
    next_token pars;
    let is_rec =
        if peek_token_type pars = REC then begin
            next_token pars;
            true
        end else false
    in
    let id = expect_id pars in
    expect pars EQ;
    let e = parse_expr pars in
    let res =
        if peek_token_type pars = IN then begin
            next_token pars;
            let body = parse_expr pars in
            if is_rec then
                LetrecIn (id, e, body)
            else
                LetIn (id, e, body)
        end else
            if is_rec then
                Letrec (id, e)
            else
                Let (id, e)
    in
    debug_parse_out "parse_let";
    res

and parse_expr pars =
    debug_parse_in "parse_expr";
    let e = match peek_token_type pars with
        | EOF -> Eof
        | LET -> parse_let pars
        | FN -> parse_fn pars
        | IF -> parse_if pars
        | BEGIN -> parse_comp pars
        | _ -> parse_logical pars
    in
    debug_parse_out "parse_expr";
    e

let parse_one tokens =
    let pars = { tokens = tokens } in
    debug_token ("token = " ^ (token_to_string @@ peek_token pars));
    parse_expr pars

let parse tokens =
    let pars = { tokens = tokens } in
    debug_token ("token = " ^ (token_to_string @@ peek_token pars));
    let rec loop res =
        let e = parse_expr pars in
        if e = Eof then
            List.rev res
        else
            loop (e::res)
    in
    loop []

