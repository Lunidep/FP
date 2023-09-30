#nowarn "40"
open System
open System.IO
open System.Collections.Generic

type Token =
    | OPERATOR of string
    | NUMBER of float
    | STRING of string
    | ID of string
    | DOT
    | COMMA
    | SEMICOLON
    | LEFT_CURLY
    | RIGHT_CURLY
    | LEFT_PARENTHESIS
    | RIGHT_PARENTHESIS

type Expr =
    | OPERATOR of string * Expr list
    | NUMBER of double
    | STRING of string
    | ID of string
    | BOOL of bool
    | COND of Expr * Expr * Expr               
    | VARIABLE of string * Expr                    
    | SET of string * Expr                    
    | FUNC_DEF of string * Expr * Expr * env * int 
    | CALL of string * Expr * int
    | PRINT of Expr
    | SIMPLE of string
    | SIMPLEOP of string
    | SIMPLELIST of Expr list
    | SIMPLEARGLIST of Expr list
and env = Map<string, Expr>

let toString = (function x -> (x |> List.toArray |> String))

// Функция преобразования списка символов в список токенов.
let tokenize source =
    let literal_tokens = Map [
        ('.', Token.DOT);
        (';', Token.SEMICOLON);
        (',', Token.COMMA);
        ('{', Token.LEFT_CURLY);
        ('}', Token.RIGHT_CURLY);
        ('(', Token.LEFT_PARENTHESIS);
        (')', Token.RIGHT_PARENTHESIS);
    ]

    let arithmetic_tokes = Map [
        ('+', Token.OPERATOR("+"));
        ('-', Token.OPERATOR("-"));
        ('*', Token.OPERATOR("*"));
        ('/', Token.OPERATOR("/"));
        ('=', Token.OPERATOR("="));
        ('>', Token.OPERATOR(">"));
        ('<', Token.OPERATOR("<"));
        ('|', Token.OPERATOR("|"));
        ('&', Token.OPERATOR("&"));
    ]

    // Вспомогательная функция для чтения строк.
    let rec read_string_end acc = function
    | '\\'::'"'::t -> (toString (List.rev acc)), t
    | '"'::t -> (toString (List.rev acc)), t
    | h::t -> read_string_end (h::acc) t
    | [] -> failwith "read_string_end ERROR: EOF before closing \" found"

    // Вспомогательная функция для чтения комментариев.
    let rec read_comment = function
    | '$'::t -> t
    | _::t -> read_comment t
    | [] -> failwith "read_comment ERROR: EOF before closing comment"

    // Вспомогательная функция для чтения однострочных комментариев.
    let rec read_linecomment = function
    | '\n'::t -> t
    | _::t -> read_linecomment t
    | [] -> []

    // Вспомогательная функция для чтения идентификаторов.
    let rec read_id acc = function
    | h::t when Char.IsWhiteSpace(h) -> (toString (List.rev acc)), t
    | h::t when Char.IsLetter(h) || Char.IsDigit(h) || h = '_' -> read_id (h::acc) t
    | h::t when h = '(' || h = ')' || h = '{' || h = '}' -> (toString (List.rev acc)), (h::t)
    | [] -> (toString (List.rev acc)), []
    | h::_ -> failwith ("read_id ERROR: Unexpected symbol met: " + (string h))

    // Вспомогательная функция для чтения чисел.
    let rec read_number acc = function
    | h::t when Char.IsWhiteSpace(h) -> (toString (List.rev acc)), t
    | h::t when Char.IsDigit(h) -> read_number (h::acc) t
    | '.'::t -> read_number ('.'::acc) t
    | h::t when h = '(' || h = ')' -> (toString (List.rev acc)), (h::t)
    | [] -> (toString (List.rev acc)), []
    | h::_ -> failwith ("read_number ERROR: Unexpected symbol met while reading digit: " + (string h))

    // Основная функция для разбора и преобразования исходного кода в список токенов.
    let rec tokenize_impl acc = function
    | h::t when Char.IsWhiteSpace(h) -> tokenize_impl acc t
    | h::t when literal_tokens |> Map.containsKey h -> tokenize_impl ((literal_tokens |> Map.find h)::acc) t
    | '"'::t | '\\'::'"'::t -> 
        let read_string, remaining_source = read_string_end [] t
        tokenize_impl (Token.STRING( read_string)::acc) remaining_source
    | '$'::t -> 
        let remaining_source = read_comment t
        tokenize_impl acc remaining_source
    | '#'::t -> 
        let remaining_source = read_linecomment t
        tokenize_impl acc remaining_source

    | h::t when Char.IsLetter(h) ->
        let read_id, remaining_source = read_id [] (h::t)
        tokenize_impl (Token.ID(read_id)::acc) remaining_source

    | h::t when Char.IsDigit(h) ->
        let read_number, remaining_source = read_number [] (h::t)
        try 
            let parsed_number = System.Double.Parse(read_number, System.Globalization.CultureInfo.InvariantCulture)
            tokenize_impl (Token.NUMBER(parsed_number)::acc) remaining_source
        with
            _ -> failwith ("tokenize_impl ERROR: Unrecognizable number met: " + read_number)
    | '-'::h::t when Char.IsDigit(h) ->
        let read_number, remaining_source = read_number [] (h::t)
        try 
            let parsed_number = System.Double.Parse("-" + read_number, System.Globalization.CultureInfo.InvariantCulture)
            tokenize_impl (Token.NUMBER(parsed_number)::acc) remaining_source
        with
            _ -> failwith ("tokenize_impl ERROR: Unrecognizable number met: " + read_number)
    | h::' '::t when (arithmetic_tokes |> Map.tryFind h).IsSome ->
         tokenize_impl ((arithmetic_tokes |> Map.find h)::acc) t

    | h::_ -> failwith ("tokenize_impl ERROR: Unsupported symbol met: " + (string h))
    | [] -> List.rev acc

    tokenize_impl [] source


// Функция для разбора списка токенов и создания абстрактного синтаксического дерева
let parse tokens = 
    let rec parse_ids acc = function
        | Token.ID(id)::t -> parse_ids (id::acc) t
        | Token.RIGHT_CURLY::t -> List.rev acc, t
        | _ -> failwith "parse_function_parameters ERROR: not found expected id"

    let key_words = ["var"; "put"; "def"; "sout";"if"; "then"; "else"]

    let rec token_parser acc = function
        | [] -> List.rev acc, []

        | Token.ID(expr)::t when (List.tryFind (fun x -> x = expr) key_words).IsSome -> token_parser (Expr.SIMPLE(expr)::acc) t

        | Token.NUMBER(n)::t -> token_parser (Expr.SIMPLELIST([Expr.NUMBER(n)])::acc) t
        | Token.ID("true")::t -> token_parser (Expr.SIMPLELIST([Expr.BOOL(true)])::acc) t
        | Token.ID("false")::t -> token_parser (Expr.SIMPLELIST([Expr.BOOL(false)])::acc) t
        | Token.ID(id)::t -> token_parser (Expr.SIMPLELIST([Expr.ID(id)])::acc) t
        | Token.STRING(s)::t -> token_parser (Expr.SIMPLELIST([Expr.STRING(s)])::acc) t

        | Token.RIGHT_CURLY::t -> List.rev acc, t
        | Token.LEFT_CURLY::t ->
            let read_args, remaining_part = token_parser [] t
            if List.forall (fun x -> match x with | Expr.SIMPLELIST([Expr.ID(_)]) -> true | _ -> false) read_args 
            then token_parser (Expr.SIMPLEARGLIST(read_args)::acc) remaining_part
            else failwith ("token_parser ERROR: non ids inside function args: " + (sprintf "%A" read_args))

        | Token.RIGHT_PARENTHESIS::t -> List.rev acc, t
        | Token.LEFT_PARENTHESIS::t ->
            let read_exprs, remaining_part = token_parser [] t
            match read_exprs with
            | Expr.SIMPLEOP(op)::t -> token_parser (Expr.SIMPLELIST([Expr.OPERATOR(op, t)])::acc) remaining_part
            | Expr.SIMPLE("var")::Expr.SIMPLELIST([Expr.ID(id)])::Expr.SIMPLELIST(list)::[] -> token_parser (Expr.SIMPLELIST([Expr.VARIABLE(id, Expr.SIMPLELIST(list))])::acc) remaining_part
            | Expr.SIMPLE("put")::Expr.SIMPLELIST([Expr.ID(id)])::Expr.SIMPLELIST(list)::[] -> token_parser (Expr.SIMPLELIST([Expr.SET(id, Expr.SIMPLELIST(list))])::acc) remaining_part

            | Expr.SIMPLE("def")::Expr.SIMPLELIST([Expr.ID(id)])::(Expr.SIMPLEARGLIST(args_list) as args)::(Expr.SIMPLELIST(_) as body)::[] -> 
                token_parser (SIMPLELIST([Expr.FUNC_DEF(id, args, body, Map<string, Expr>[], List.length args_list)])::acc) remaining_part

            | Expr.SIMPLELIST([Expr.ID(id)])::t ->
                if List.forall (fun x -> match x with | Expr.SIMPLELIST(_) -> true | _ -> false) t
                then token_parser (SIMPLELIST([Expr.CALL(id, Expr.SIMPLELIST(t), List.length t)])::acc) remaining_part
                else failwith ("token_parser ERROR: wrong function syntax! Used as parameters for function call: " + (sprintf "%A" t)) 

            | Expr.SIMPLELIST(list)::t -> 
                let rec gather_simple_lists acc = function
                | (Expr.SIMPLELIST(_) as list)::t' -> gather_simple_lists (list::acc) t'
                | [] -> List.rev acc, []
                | waste -> 
                    printfn "unmatched expr: %A" waste
                    failwith "gather_simple_lists ERROR: misformat expression"

                let lists, _ = gather_simple_lists [] (SIMPLELIST(list)::t)
                token_parser (Expr.SIMPLELIST(lists)::acc) remaining_part
            | Expr.SIMPLE("if")::(Expr.SIMPLELIST(_) as cond)
                ::Expr.SIMPLE("then")::(Expr.SIMPLELIST(_) as expr1)
                ::Expr.SIMPLE("else")::(Expr.SIMPLELIST(_) as expr2)::[] -> 
                    token_parser (Expr.SIMPLELIST([Expr.COND(cond, expr1, expr2)])::acc) remaining_part

            | Expr.SIMPLE("if")::(Expr.SIMPLELIST(_) as cond)
                ::Expr.SIMPLE("then")::(Expr.SIMPLELIST(_) as expr1)::[] ->
                    token_parser (Expr.SIMPLELIST([Expr.COND(cond, expr1, SIMPLE(""))])::acc) remaining_part

            | Expr.SIMPLE("sout")::(Expr.SIMPLELIST(_) as body)::[] -> token_parser (Expr.SIMPLELIST([Expr.PRINT(body)])::acc) remaining_part

            | (Expr.NUMBER(_) as num_expr)::[] -> token_parser (SIMPLELIST([num_expr])::acc) remaining_part
            | (Expr.STRING(_) as str_expr)::[] -> token_parser (SIMPLELIST([str_expr])::acc) remaining_part
            | waste -> failwith ("token_parser ERROR: wrong parenthesis structure: " + (sprintf "%A" waste))

        | Token.OPERATOR(op)::t -> token_parser (Expr.SIMPLEOP(op)::acc) t
        | waste -> 
            printfn "unexpected token: %A" waste
            failwith "token_parser ERROR: unexpected token"

    let parsed_expr, remaining_part = token_parser [] tokens
    if remaining_part <> [] then 
        printfn "unparsed part: %A" remaining_part
        failwith "token_parser ERROR: misformat expression"
    Expr.SIMPLELIST(parsed_expr)

// Функция для интерпретации дерева выражения.
let evaluate  env expr = 
    let lookup name env = env |> Map.find name

    let numeric_operators = Map [
        ("+", ((function x -> x), (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.NUMBER(x + y))));
        ("-", ((function Expr.NUMBER(x) -> Expr.NUMBER(-x)), (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.NUMBER(x - y))));
        ("*", ((function x -> x), (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.NUMBER(x * y))));
        ("/", ((function Expr.NUMBER(x) -> Expr.NUMBER(1. / x)), (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.NUMBER(x / y))));
    ]
    let bool_operators = Map [
        ("&", ((function x -> x), (function (Expr.BOOL(x), Expr.BOOL(y)) -> Expr.BOOL(x && y))));
        ("|", ((function x -> x), (function (Expr.BOOL(x), Expr.BOOL(y)) -> Expr.BOOL(x || y))));
    ]
    let binary_operators = Map [
        (">", (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.BOOL(x > y)));
        ("<", (function (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.BOOL(x < y)));
    ]
    // Вспомогательные функции для интерпретации
    let rec eval_args_bool eval_fn env acc = fun x ->
        match x with
        | h::t -> 
            let evaluated, new_env = eval_fn env h
            match evaluated with
            | Expr.BOOL(_) -> 
                eval_args_bool eval_fn new_env (evaluated::acc) t
            | Expr.SIMPLELIST([Expr.BOOL(_) as boolean]) ->
                eval_args_bool eval_fn new_env (boolean::acc) t
            | Expr.NUMBER(n) -> 
                eval_args_bool eval_fn new_env (Expr.BOOL(Convert.ToBoolean n)::acc) t
            | Expr.SIMPLELIST([Expr.NUMBER(n)]) ->
                eval_args_bool eval_fn new_env (Expr.BOOL(Convert.ToBoolean n)::acc) t
            | waste -> failwith ("check_bool ERROR: unevaluatable bool expression: " + (sprintf "%A" waste))
        | [] -> List.rev acc, env

    let rec eval_args_num eval_fn env acc = fun x ->
        match x with
        | h::t -> 
            let evaluated, new_env = eval_fn env h
            match evaluated with
            | Expr.NUMBER(_) -> 
                eval_args_num eval_fn new_env (evaluated::acc) t
            | Expr.SIMPLELIST([Expr.NUMBER(_) as number]) ->
                eval_args_num eval_fn new_env (number::acc) t
            | waste -> failwith ("check_number ERROR: unevaluatable numeric expression: " + (sprintf "%A" waste))
        | [] -> List.rev acc, env

    let rec eval_impl env = function
        | Expr.NUMBER(_) as number -> number, env
        | Expr.STRING(_) as string -> string, env
        | Expr.BOOL(_) as boolean -> boolean, env
        | Expr.ID(id) -> eval_impl env (lookup id env)

        | Expr.SIMPLELIST([Expr.NUMBER(_) as number]) -> number, env
        | Expr.SIMPLELIST([Expr.STRING(_) as string]) -> string, env
        | Expr.SIMPLELIST([Expr.BOOL(_) as boolean]) -> boolean, env
        | Expr.SIMPLELIST([Expr.ID(id)]) -> lookup id env, env

        | Expr.SIMPLELIST([Expr.OPERATOR(_) as op]) -> eval_impl env op

        | Expr.OPERATOR(op, t) when (Map.tryFind op numeric_operators).IsSome ->
            let (single_lambda, multiple_lambda) = Map.find op numeric_operators
            let evaluated_list, new_env = eval_args_num eval_impl env [] t
            match List.length evaluated_list with
            | 0 -> failwith "eval_impl ERROR: operator + can't have 0 arguments"
            | 1 -> single_lambda (List.head evaluated_list), new_env
            | _ -> List.reduce (fun x y -> multiple_lambda (x, y)) evaluated_list, new_env
        | Expr.OPERATOR(op, t) when (Map.tryFind op bool_operators).IsSome ->
            let (single_lambda, multiple_lambda) = Map.find op bool_operators
            let evaluated_list, new_env = eval_args_bool eval_impl env [] t
            match List.length evaluated_list with
            | 0 -> failwith "eval_impl ERROR: operator + can't have 0 arguments"
            | 1 -> single_lambda (List.head evaluated_list), new_env
            | _ -> List.reduce (fun x y -> multiple_lambda (x, y)) evaluated_list, new_env

        | Expr.OPERATOR("=", t) ->
            match List.length t with
            | 2 ->
                let first::second::[] = t
                let eval_first, new_env = eval_impl env first
                let eval_second, new_env' = eval_impl new_env second
                (function 
                    | (Expr.NUMBER(x), Expr.NUMBER(y)) -> Expr.BOOL(x = y)
                    | (Expr.BOOL(x), Expr.BOOL(y)) -> Expr.BOOL(x = y)
                    | (Expr.STRING(x), Expr.STRING(y)) -> Expr.BOOL(x = y)
                    | _ -> failwith ("eval_impl ERROR: given unsupported arguments: " + (sprintf "%A %A" eval_first eval_second))
                ) (eval_first, eval_second), new_env'
            | _ -> failwith ("eval_impl ERROR: operator " + (sprintf "%s" "=") + " can't have not 2 arguments")
        | Expr.OPERATOR(op, t) when (Map.tryFind op binary_operators).IsSome ->
            let evaluated_list, new_env = eval_args_num eval_impl env [] t
            let functor = Map.find op binary_operators
            match List.length evaluated_list with
            | 2 -> 
                let first::second::[] = evaluated_list
                functor (first, second), new_env
            | _ -> failwith ("eval_impl ERROR: operator " + (sprintf "%s" op) + " can't have not 2 arguments")

        | Expr.VARIABLE(id, list) -> Expr.SIMPLE (""), (Map.add id list env)
        | Expr.SET(id, list) -> Expr.SIMPLE (""), (Map.add id list env)
        | Expr.COND(cond, expr1, expr2) ->
            let eval_cond, new_env = eval_impl env cond
            match eval_cond with 
            | Expr.NUMBER(n) -> if Convert.ToBoolean n then (expr1, new_env) else (expr2, new_env)
            | Expr.BOOL(b) -> if b then (expr1, new_env) else (expr2, new_env)
            | waste -> failwith ("eval_impl ERROR: unevaluatable cond expression: " + (sprintf "%A" waste))
        | Expr.SIMPLELIST(list) -> 
            let rec eval_lists env = function
                | h::t -> 
                    let evaluated_first, new_env = eval_impl env h
                    match evaluated_first with
                    | Expr.SIMPLE(_) as simple -> eval_lists new_env t
                    | _ ->
                        let evaluated, new_env' = eval_impl new_env evaluated_first 
                        match evaluated with
                        | Expr.NUMBER(_) | Expr.STRING(_) | Expr.BOOL(_)-> 
                            if List.length t <> 0 then printfn "eval_simple_lists@ warning# useless members at the end of list"
                            evaluated, new_env'
                        | _ -> eval_lists new_env' t
                | [] ->
                    Expr.SIMPLE(""), env
            eval_lists env list
        | Expr.FUNC_DEF(id, args, body, _, arity) ->
            Expr.SIMPLE(""), (Map.add id (Expr.FUNC_DEF(id, args, body, env, arity)) env)
        | Expr.CALL(id, Expr.SIMPLELIST(args), arity) ->
            let env_function = Map.tryFind id env
            if env_function.IsNone then failwith ("eval_impl ERROR: use of undeclared function " + id)
            else 
                let (Expr.FUNC_DEF(_, Expr.SIMPLEARGLIST(env_args), body, env_env, env_arity)) = env_function.Value
                if arity <> env_arity 
                then failwith ("eval_impl ERROR: function use with different arity: expected " + (sprintf "%A" env_arity) + " got: " + (sprintf "%A" arity))
                else
                    let rec add_env_args env = function
                    | (Expr.SIMPLELIST([Expr.ID(h1)])::t1), (h2::t2) -> 
                        let eval_h2, new_env =  eval_impl env h2
                        add_env_args (Map.add h1 eval_h2 new_env) (t1, t2)
                    | ([], []) -> env
                    | waste -> failwith ("eval_impl ERROR: Some serious thing happened diring concatenations of maps: " + (sprintf "%A" waste))

                    let new_env = add_env_args env (env_args, args) 
                    let merged_env = Map.fold (fun acc key value -> Map.add key value acc) env new_env
                    let merged_env2 = Map.fold (fun acc key value -> Map.add key value acc) merged_env env_env

                    eval_impl merged_env2 body
        | Expr.PRINT(body) ->
            let evaludated_body, new_env = eval_impl env body
            match evaludated_body with
            | Expr.SIMPLE(_) -> failwith ("eval_impl.sout ERROR: unevaluatable simple value to sout\n")
            | _ ->
                let evaluated_eval, new_env' = eval_impl new_env evaludated_body
                match evaluated_eval with
                | Expr.NUMBER(num) -> 
                    printfn "%f" num
                    SIMPLE(""), new_env
                | Expr.STRING(str) -> 
                    printfn "\"%s\"" str
                    SIMPLE(""), new_env
                | Expr.BOOL(boolean) ->
                    if boolean then
                        printfn "true"
                        SIMPLE(""), new_env
                    else
                        printfn "false"
                        SIMPLE(""), new_env
                | Expr.ID(id_val) as id ->
                    let evaluated_id, new_env'' = eval_impl new_env' id
                    printfn "id: %s = %A" id_val evaluated_id
                    SIMPLE(""), new_env''
        | Expr.SIMPLE(_) -> failwith ("eval_impl ERROR: simple value is invaluatable\n")
        | waste -> failwith ("eval_impl ERROR: wrong structure to evaluate" + (sprintf "%A\n" waste))

    match expr with
    | h -> eval_impl env h

let env  =  Map<String, Expr> []

let rec repl env =
    printf "> "
    let source = Console.ReadLine()
    try
        printfn "Source: %s" source
        let tokens = tokenize (source |> Seq.toList)
        let expr = parse tokens
        printfn "parsed: %A" expr
        let evaluated, new_env = evaluate  env expr
        printfn "Processed: %A\n" evaluated
        repl new_env
    with ex ->
        printfn "Exception: %s" ex.Message
        repl env


let rec execute' env  = function filename ->
    let lines = File.ReadAllLines(filename)
    let source = String.concat "" lines
    try
        printfn "Provided code: %s" source
        let tokens = tokenize (source |> Seq.toList)
        let expr = parse tokens
        let evaluated, new_env = evaluate  env expr
        printfn "Processed: %A\n" evaluated
    with ex ->
        printfn "Exception: %s" ex.Message
        repl env

let execute filename = execute' env filename

execute "sample.x" // put name of your file here instead of "sample.x"