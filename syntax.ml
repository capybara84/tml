

exception Error of string

type token_type
    = EOF | ID of string | BOOL_LIT of bool | INT_LIT of int
    | CHAR_LIT of char | STRING_LIT of string
    | LET | REC | IN | FN | IF | THEN | ELSE
    | EQ | EQL | NEQ | LT | LE | GT | GE | MINUS | PLUS | SLASH | STAR
    | NOT | OR | LOR | LAND | ARROW | LPAR | RPAR
    | COMMA | SEMI

type token = {
    token_type : token_type;
    line : int;
}

type id = string

type binop = BinAdd | BinSub | BinMul | BinDiv | BinLT | BinLE | BinGT | BinGE
            | BinEql | BinNeq | BinLor | BinLand
type unop = UNot | UMinus

type exp = Eof | EInt of int | EBool of bool | Ident of id
    | EChar of char | EString of string
    | Binary of binop * exp * exp | Unary of unop * exp
    | If of exp * exp * exp
    | Let of id * exp * exp | Letrec of id * exp * exp
    | Fn of id * exp | Apply of exp * exp

type value = VUnit | VInt of int | VBool of bool | VChar of char | VString of string
    | Closure of id * exp * env_t
and env_t = (id * value ref) list

let env_extend env id value = (id, value) :: env
let env_lookup env id = List.assoc id env

let token_type_to_string = function
    | EOF -> "<EOF>"
    | ID id -> id | BOOL_LIT b -> string_of_bool b | INT_LIT n -> string_of_int n
    | CHAR_LIT c -> "'" ^ String.make 1 c ^ "'" | STRING_LIT s -> "\"" ^ s ^ "\""
    | LET -> "let" | REC -> "rec" | IN -> "in" | FN -> "fn"
    | IF -> "if" | THEN -> "then" | ELSE -> "else"
    | EQ -> "=" | EQL -> "==" | NEQ -> "!=" | LT -> "<"
    | LE -> "<=" | GT -> ">" | GE -> ">=" | MINUS -> "-" | PLUS -> "+"
    | SLASH -> "/" | STAR -> "*" | NOT -> "!" | OR -> "|"
    | LOR -> "||" | LAND -> "&&" | ARROW -> "->" | LPAR -> "(" | RPAR -> ")"
    | COMMA -> "," | SEMI -> ";"

let token_to_string t = token_type_to_string t.token_type

let str_of_binop = function
    | BinAdd -> "+" | BinSub -> "-" | BinMul -> "*"
    | BinDiv -> "/" | BinLT -> "<" | BinLE -> "<="
    | BinGT -> ">" | BinGE -> ">=" | BinEql -> "=="
    | BinNeq -> "!=" | BinLor -> "||" | BinLand -> "&&"
let str_of_unop = function
    | UNot -> "!"
    | UMinus -> "-"

let rec exp_to_str = function
    | Eof -> "<EOF>"
    | EInt n -> "EInt " ^ string_of_int n
    | EBool b -> "EBool " ^ string_of_bool b
    | Ident id -> "Ident \"" ^ id ^ "\""
    | EChar c -> "EChar " ^ String.make 1 c
    | EString s -> "EString \"" ^ s ^ "\""
    | Binary (op, x, y) -> "(" ^ exp_to_str x ^ " " ^ str_of_binop op ^ " " ^ exp_to_str y ^ ")"
    | Unary (op, e) -> "(" ^ str_of_unop op ^ exp_to_str e ^ ")"
    | If (c, t, e) -> "(if " ^ exp_to_str c ^ " then " ^ exp_to_str t ^ " else " ^ exp_to_str e ^ ")"
    | Let (id, e, b) -> "(let " ^ id ^ " = " ^ exp_to_str e ^ " in " ^ exp_to_str b ^ ")"
    | Letrec (id, e, b) -> "(let rec " ^ id ^ " = " ^ exp_to_str e ^ " in " ^ exp_to_str b ^ ")"
    | Fn (a, b) -> "(fn " ^ a ^ " -> " ^ exp_to_str b ^ ")"
    | Apply (f, a) -> "(" ^ exp_to_str f ^ " " ^ exp_to_str a ^ ")"

let value_to_str = function
    | VUnit -> "()"
    | VInt n -> string_of_int n
    | VBool b -> string_of_bool b
    | VChar c -> String.make 1 c
    | VString s -> s
    | Closure (arg, body, _) -> "<fn " ^ arg ^ " -> " ^ exp_to_str body ^ ">" 

