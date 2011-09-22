open Format
open Ast

let mk_sep sep =
  let first = ref true in
    fun fmt ->
      if !first then
        first := false
      else
        fprintf fmt sep

let pp_char = pp_print_char
let pp_string = pp_print_string
let pp_int = pp_print_int
let pp_float = pp_print_float

let pp_fixme fmt = pp_string fmt "fixme"

let pp_empty fmt _ = ()

let pp_paren pp fmt = fprintf fmt "(%a)" pp

let pp_opt pp fmt = function
  | None -> ()
  | Some x -> pp fmt x

let rec pp_list ?(sep=", ") pp fmt = function
  | [] -> ()
  | [x] -> pp fmt x
  | x :: xs ->
      fprintf fmt "%a%s%a"
        pp x sep (pp_list pp) xs

let rec pp_block pp fmt = function
  | [] -> ()
  | [x] -> pp fmt x
  | x :: xs ->
      fprintf fmt "%a@\n%a"
        pp x (pp_block pp) xs

let rec pp_stmt fmt stmt = pp_expr' true fmt stmt
and pp_suite fmt = pp_block pp_stmt fmt

and pp_expr fmt = pp_expr' false fmt
and pp_expr' stmt fmt = function
  | Empty (_) -> if not stmt then pp_string fmt "()"

  | Alias (new_id, old_id, _) ->
      fprintf fmt "alias %s %s" new_id old_id

  | Undef (ids, _) ->
      fprintf fmt "undef %a"
        (pp_list ~sep:" " pp_string) ids

  | Defined (e, _) ->
      fprintf fmt "defined %a" pp_expr e

  | Nil _ -> pp_string fmt "nil"
  | True _ -> pp_string fmt "true"
  | False _ -> pp_string fmt "false"
  | Self _ -> pp_string fmt "self"

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

  | Preexec (e, _) ->
      fprintf fmt "@[<2>BEGIN {@\n%a@]@\n}" pp_stmt e
        
  | Postexec (e, _) ->
      fprintf fmt "@[<2>END {@\n%a@]@\n}" pp_stmt e

  | Block (stmts, _) ->
      if stmt then
        pp_suite fmt stmts
      else
        pp_paren pp_suite fmt stmts

  | Begin ({ body = e;
             body_rescues = res;
             body_else = els;
             body_ensure = ens }, _) ->
      fprintf fmt "@[<2>begin@\n%a@]@\n" pp_stmt e;
      List.iter
        (fun (types, e) ->
           fprintf fmt "@[<2>rescue";
           if types <> [] then
             fprintf fmt " %a" pp_expr_list types;
           fprintf fmt "@\n%a@]@\n" pp_stmt e)
        res;
      if els <> Empty then
        fprintf fmt "@[<2>else@\n%a@]@\n" pp_stmt els;
      if ens <> Empty then
        fprintf fmt "@[<2>ensure@\n%a@]@\n" pp_stmt ens;
      pp_string fmt "end"

  | Not (e, _) ->
      fprintf fmt "(not %a)" pp_expr e

  | And (e1, e2, _) ->
      pp_binop fmt e1 "&&" e2

  | Or (e1, e2, _) ->
      pp_binop fmt e1 "||" e2

  | Flip2 (e1, e2, _) ->
      pp_binop fmt e1 ".." e2

  | Flip3 (e1, e2, _) ->
      pp_binop fmt e1 "..." e2

  | If (c, t, e, _) ->
      fprintf fmt "@[<2>if %a@\n%a@]@\n"
        pp_expr c
        pp_stmt t;
      if e <> Empty then
        fprintf fmt "@[<2>else@\n%a@]@\n" pp_stmt e;
      pp_string fmt "end"

  | While (c, e, pre, _) ->
      if pre then
        fprintf fmt "%a while %a"
          pp_expr c
          pp_expr e
      else
        fprintf fmt "@[<2>while %a@\n%a@]@\nend"
          pp_expr c
          pp_stmt e

  | Until (c, e, pre, _) ->
      if pre then
        fprintf fmt "%a until %a"
          pp_expr c
          pp_expr e
      else
        fprintf fmt "@[<2>until %a@\n%a@]@\nend"
          pp_expr c
          pp_stmt e

  | For (gen, vars, e, _) ->
      pp_fixme fmt

  | Case ({ case_expr = e;
            case_whens = whens;
            case_else = els }, _) ->
      fprintf fmt "@[<2>case %a@]@\n" pp_expr e;
      List.iter
        (fun (guards, e) ->
           fprintf fmt "@[<2>when %a@\n%a@]@\n"
             pp_expr_list guards
             pp_stmt e)
        whens;
      if els <> Empty then
        fprintf fmt "@[<2>else@\n%a@]@\n" pp_stmt els;
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
             pp_paren_expr_list fmt args
      end

  | Iter (call, args, e, _) ->
      fprintf fmt "@[<2>%a {" pp_expr call;
      if args <> [] then
        fprintf fmt "|%a|" pp_expr_list args;
      fprintf fmt "@\n%a@]@\n}" pp_stmt e

  | Block_pass (e, _) ->
      pp_char fmt '&';
      pp_expr fmt e

  | Return (args, _) ->
      fprintf fmt "return %a" pp_expr_list args

  | Yield (args, _) ->
      fprintf fmt "yield %a" pp_expr_list args

  | Super (args, _) ->
      fprintf fmt "yield%a" pp_paren_expr_list args

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

  | Lvar (id, _)
  | Dvar (id, _) ->
      pp_string fmt id
  | Ivar (id, _)
  | Cvar (id, _)
  | Gvar (id, _) ->
      pp_string fmt id

  | Cdecl (id, e, _)
  | Lasgn (id, e, _)
  | Dasgn (id, e, _)
  | Iasgn (id, e, _)
  | Cvasgn (id, e, _)
  | Cvdecl (id, e, _)
  | Gasgn (id, e, _) ->
      pp_string fmt id;
      if e <> Empty then
        (* TODO *)
        fprintf fmt " = %a" pp_expr e

  | Masgn (lhs, rhs, _) ->
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

  | Class (path, super, e, _) ->
      fprintf fmt "@[<2>class %a" pp_expr path;
      if super <> Empty then
        fprintf fmt " < %a" pp_expr super;
      fprintf fmt "@\n%a@]@\nend" pp_stmt e

  | Sclass (recv, e, _) ->
      fprintf fmt "@[<2>class << %a@\n%a@]@\nend"
        pp_expr recv
        pp_stmt e

  | Module (path, e, _) ->
      fprintf fmt "@[<2>module %a@\n%a@]@\nend"
        pp_expr path
        pp_stmt e

  | Defn (id, params, e, _) ->
      fprintf fmt "@[<2>def %s(%a)@\n%a@]@\nend"
        id
        (pp_list pp_param) params
        pp_stmt e

  | Defs (recv, id, params, e, _) ->
      fprintf fmt "@[<2>def %a.%s(%a)@\n%a@]@\nend"
        pp_expr recv
        id
        (pp_list pp_param) params
        pp_stmt e

and pp_expr_list fmt = pp_list pp_expr fmt
and pp_paren_expr_list fmt = pp_paren pp_expr_list fmt

and pp_string_contents fmt ?(delim='"') contents =
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

and pp_binop fmt lhs op rhs =
  fprintf fmt "(@[%a %s %a@])"
    pp_expr lhs op pp_expr rhs

let pp_print_expr fmt expr = pp_expr fmt expr
let print_expr expr = pp_print_expr std_formatter expr
