open Format
open Ast

let pp_char = pp_print_char
let pp_string = pp_print_string
let pp_int = pp_print_int
let pp_float = pp_print_float

let pp_fixme fmt = pp_string fmt "fixme"

let rec pp_list ?(sep=", ") pp fmt = function
  | [] -> ()
  | [x] -> pp fmt x
  | x :: xs ->
      fprintf fmt "%a%s%a"
        pp x sep (pp_list pp) xs

let rec pp_string_contents fmt ?(delim='"') contents =
  pp_char fmt delim;
  List.iter
    (function
     | Str_contents s ->
         pp_string fmt (String.escaped s)
     | Str_interpol e ->
         fprintf fmt "#{%a}" pp_expr e)
    contents;
  pp_char fmt delim

and pp_hash fmt = function
  | [] -> ()
  | k :: v :: xs ->
      fprintf fmt "%a => %a"
        pp_expr k pp_expr v;
      if xs <> [] then
        pp_string fmt ", ";
      pp_hash fmt xs
  | _ ->
      pp_fixme fmt

and pp_binop fmt lhs op rhs =
  fprintf fmt "(@[%a %s %a@])"
    pp_expr lhs op pp_expr rhs

and pp_param fmt = function
  | Param_id id ->
      pp_string fmt id
  | Param_opt (id, e) ->
      fprintf fmt "%s = %a"
        id pp_expr e
  | Param_rest id ->
      fprintf fmt "*%s" id
  | Param_star ->
      pp_char fmt '*'
  | Param_block id ->
      fprintf fmt "&%s" id

and pp_body_stmt fmt { body = body;
                       body_rescues = rescues;
                       body_else = elsbody;
                       body_ensure = ensbody } =
  fprintf fmt "%a@]@\n" pp_body body;
  List.iter
    (fun (types, resbody) ->
       fprintf fmt "@[<2>rescue";
       if types <> [] then
         fprintf fmt " %a" pp_expr_list types;
       fprintf fmt "@\n%a@]@\n" pp_body resbody)
    rescues;
  if elsbody <> [] then
    fprintf fmt "@[<2>else@\n%a@]@\n" pp_body elsbody;
  if ensbody <> [] then
    fprintf fmt "@[<2>ensure@\n%a" pp_body ensbody;

and pp_stmt fmt = function
  | Alias (new_id, old_id, _) ->
      fprintf fmt "alias %s %s" new_id old_id
  | Undef (ids, _) ->
      fprintf fmt "undef %a"
        (pp_list ~sep:" " pp_string) ids

  | If_mod (body, test, _) ->
      fprintf fmt "%a if %a"
        pp_stmt body
        pp_expr test
  | Unless_mod (body, test, _) ->
      fprintf fmt "%a unless %a"
        pp_stmt body
        pp_expr test
  | While_mod (body, test, _) ->
      fprintf fmt "%a while %a"
        pp_stmt body
        pp_expr test
  | Until_mod (body, test, _) ->
      fprintf fmt "%a until %a"
        pp_stmt body
        pp_expr test
  | Rescue_mod (body, resbody, _) ->
      fprintf fmt "%a rescue %a"
        pp_stmt body
        pp_stmt resbody

  | Pre_exec (body, _) ->
      fprintf fmt "@[<2>BEGIN {@\n%a@]@\n}"
        pp_body body
  | Post_exec (body, _) ->
      fprintf fmt "@[<2>END {@\n%a@]@\n}"
        pp_body body

  | Expr (expr, _) ->
      pp_expr fmt expr

and pp_body fmt = function
  | [] -> ()
  | [x] -> pp_stmt fmt x
  | x :: xs ->
      fprintf fmt "%a@\n%a"
        pp_stmt x
        pp_body xs

and pp_expr fmt = function
  | Empty (_) -> ()

  | Literal (Lit_string contents, _) ->
      pp_string_contents fmt contents
  | Literal (Lit_xstring contents, _) ->
      pp_string_contents fmt contents ~delim:'`'
  | Literal (Lit_symbol contents, _) ->
      pp_char fmt ':';
      pp_string_contents fmt contents;
  | Literal (Lit_integer int, _) ->
      pp_int fmt int
  | Literal (Lit_float float, _) ->
      pp_float fmt float
  | Literal (Lit_regexp (contents, _), _) ->
      pp_string_contents fmt contents ~delim:'/'

  | Identifier (id, _) ->
      begin match id with
      | Id_local id         -> pp_string fmt id
      | Id_dynamic id       -> pp_string fmt id
      | Id_instance id      -> fprintf fmt "@%s" id
      | Id_class id         -> fprintf fmt "@@%s" id
      | Id_global id        -> fprintf fmt "$%s" id
      | Id_constant id      -> pp_string fmt id
      | Id_pseudo Pid_nil   -> pp_string fmt "nil"
      | Id_pseudo Pid_true  -> pp_string fmt "true"
      | Id_pseudo Pid_false -> pp_string fmt "false"
      | Id_pseudo Pid_self  -> pp_string fmt "self"
      end

  | Array (es, _) ->
      fprintf fmt "[@[%a]@]" pp_expr_list es

  | Splat (e, _) ->
      fprintf fmt "*%a" pp_expr e

  | Svalue (es, _) ->
      pp_expr_list fmt es

  | Hash (hash, _) ->
      fprintf fmt "{%a}" pp_hash hash

  | Dot2 (e1, e2, _) ->
      pp_binop fmt e1 ".." e2

  | Dot3 (e1, e2, _) ->
      pp_binop fmt e1 "..." e2

  | Begin (body, _) ->
      fprintf fmt "@[<2>begin@\n%a@]@\nend" pp_body_stmt body

  | Not (e, _) ->
      fprintf fmt "(not %a)" pp_expr e
  | And (e1, e2, _) ->
      pp_binop fmt e1 "&&" e2
  | Or (e1, e2, _) ->
      pp_binop fmt e1 "||" e2

  | Defined (e, _) ->
      fprintf fmt "defined %a" pp_expr e

  | Ternary (test, thenexpr, elsexpr, _) ->
      fprintf fmt "%a ? %a : %a"
        pp_expr test
        pp_expr thenexpr
        pp_expr elsexpr
  | If (test, thenbody, elsbody, _) ->
      fprintf fmt "@[<2>if %a@\n%a@]@\n"
        pp_expr test
        pp_body thenbody;
      fprintf fmt "@[<2>else@\n%a@]@\n"
        pp_body elsbody;
      pp_string fmt "end"
  | Unless (test, thenbody, elsbody, _) ->
      fprintf fmt "@[<2>unless %a@\n%a@]@\n"
        pp_expr test
        pp_body thenbody;
      fprintf fmt "@[<2>else@\n%a@]@\n"
        pp_body elsbody;
      pp_string fmt "end"
  | While (test, body, _) ->
      fprintf fmt "@[<2>while %a@\n%a@]@\nend"
        pp_expr test
        pp_body body
  | Until (test, body, _) ->
      fprintf fmt "@[<2>until %a@\n%a@]@\nend"
        pp_expr test
        pp_body body
  | For (gen, vars, e, _) ->
      pp_fixme fmt
  | Case ({ case_expr = expr;
            case_whens = whens;
            case_else = elsbody }, _) ->
      fprintf fmt "@[<2>case %a@]@\n" pp_expr expr;
      List.iter
        (fun (guards, whenbody) ->
           fprintf fmt "@[<2>when %a@\n%a@]@\n"
             pp_expr_list guards
             pp_body whenbody)
        whens;
      fprintf fmt "@[<2>else@\n%a@]@\n" pp_body elsbody;
      pp_string fmt "end"
  | Break (args, _) ->
      fprintf fmt "break %a" pp_expr_list args
  | Next (args, _) ->
      fprintf fmt "next %a" pp_expr_list args
  | Redo _ -> pp_string fmt "redo"
  | Retry _ -> pp_string fmt "retry"

  | Call (recv, id, args, _) ->
      begin match id with
       | "|" | "^" | "&" | "<=>" | "=="
       | "===" | "=~" | ">" | ">=" | "<"
       | "<=" | "<<" | ">>" | "+" | "-"
       | "*" | "/" | "%" | "**" when List.length args = 1 ->
           pp_binop fmt recv id (List.hd args)
       | "~" | "+@" | "-@" when args = [] ->
           pp_char fmt id.[0];
           pp_expr fmt recv
       | "[]" ->
           fprintf fmt "%a[%a]"
             pp_expr recv
             pp_expr_list args
       | _ ->
           if recv <> Empty then
             fprintf fmt "%a." pp_expr recv;
           pp_string fmt id;
           if args <> [] then
             fprintf fmt "(%a)" pp_expr_list args
      end

  | Iter (call, args, body, _) ->
      fprintf fmt "@[<2>%a {" pp_expr call;
      if args <> [] then
        fprintf fmt "|%a|" pp_expr_list args;
      fprintf fmt "@\n%a@]@\n}" pp_body body

  | Block_pass (e, _) ->
      pp_char fmt '&';
      pp_expr fmt e

  | Return (args, _) ->
      fprintf fmt "return %a" pp_expr_list args

  | Yield (args, _) ->
      fprintf fmt "yield %a" pp_expr_list args

  | Super (args, _) ->
      fprintf fmt "yield(%a)" pp_expr_list args

  | Zsuper _ ->
      pp_string fmt "super"

  | Const (id, _) ->
      pp_string fmt id

  | Colon2 (path, id, _) ->
      if path <> Empty then
        fprintf fmt "%a::" pp_expr path;
      pp_string fmt id

  | Colon3 (id, _) ->
      fprintf fmt "::%s" id

  | Declare (id, expr, _)
  | Assign (id, expr, _) ->
      fprintf fmt "%s = %a"
        (string_of_identifier id)
        pp_expr expr
  | Massign (lhs, rhs, _) ->
      pp_fixme fmt

  | Op_asgn1 (recv, args, op, e, _) ->
      fprintf fmt "%a[%a] %s= %a"
        pp_expr recv
        pp_expr_list args
        op
        pp_expr e

  | Op_asgn2 (recv, id, op, e, _) ->
      fprintf fmt "%a.%s %s= %a"
        pp_expr recv
        id op
        pp_expr e

  | Op_asgn (recv, e, id, op, _) ->
      fprintf fmt "%a::%s %s= %a"
        pp_expr recv
        id op
        pp_expr e

  | Op_asgn_or (recv, e, _) ->
      fprintf fmt "%a ||= %a"
        pp_expr recv
        pp_expr e

  | Op_asgn_and (recv, e, _) ->
      fprintf fmt "%a &&= %a"
        pp_expr recv
        pp_expr e

  | Attrasgn (e, id, es, _) ->
      pp_fixme fmt

  | Class (path, super, body, _) ->
      fprintf fmt "@[<2>class %a" pp_expr path;
      if super <> Empty then
        fprintf fmt " < %a" pp_expr super;
      fprintf fmt "@\n%a@]@\nend" pp_body_stmt body

  | Sclass (recv, body, _) ->
      fprintf fmt "@[<2>class << %a@\n%a@]@\nend"
        pp_expr recv
        pp_body_stmt body

  | Module (path, body, _) ->
      fprintf fmt "@[<2>module %a@\n%a@]@\nend"
        pp_expr path
        pp_body_stmt body

  | Defn (id, params, body, _) ->
      fprintf fmt "@[<2>def %s(%a)@\n%a@]@\nend"
        id
        (pp_list pp_param) params
        pp_body_stmt body

  | Defs (recv, id, params, body, _) ->
      fprintf fmt "@[<2>def %a.%s(%a)@\n%a@]@\nend"
        pp_expr recv
        id
        (pp_list pp_param) params
        pp_body_stmt body

  | Block (body, _) ->
      fprintf fmt "(%a)" pp_body body

and pp_expr_list fmt = pp_list pp_expr fmt

let pp_print_stmt fmt stmt = pp_stmt fmt stmt
let pp_print_body fmt stmt = pp_body fmt stmt
let pp_print_expr fmt expr = pp_expr fmt expr
let print_stmt stmt = pp_print_stmt std_formatter stmt
let print_body stmt = pp_print_body std_formatter stmt
let print_expr expr = pp_print_expr std_formatter expr
