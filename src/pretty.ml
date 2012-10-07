open Format
open Ast

let pp_char = pp_print_char
let pp_string = pp_print_string
let pp_int = pp_print_int
let pp_float = pp_print_float

let pp_fixme fmt = pp_string fmt "fixme"

let pp_opt pp fmt = function
  | None -> ()
  | Some x -> pp fmt x

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

and pp_var fmt = function
  | Var_const cpath -> pp_cpath fmt cpath
  | var -> pp_string fmt (string_of_variable var)

and pp_cpath fmt = function
  | Cpath_name (name) ->
      pp_string fmt name
  | Cpath_rel (parent, name) ->
      fprintf fmt "%a::%s" pp_expr parent name
  | Cpath_glob (cpath) ->
      fprintf fmt "::%a" pp_cpath cpath

and pp_param fmt = function
  | Param_req id ->
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

and pp_arg fmt = function
  | Arg_value e ->
      pp_expr fmt e
  | Arg_splat e ->
      fprintf fmt "*%a" pp_expr e
  | Arg_hash assocs ->
      pp_hash fmt assocs
  | Arg_block e ->
      fprintf fmt "&%a" pp_expr e

and pp_arg_list fmt = pp_list pp_arg fmt

and pp_lhs fmt = function
  | Lhs_var var
  | Lhs_decl var ->
      pp_var fmt var
  | Lhs_dstr dstr ->
      fprintf fmt "(%a)" (pp_list pp_lhs) dstr
  | Lhs_rest rest ->
      fprintf fmt "*%a" pp_lhs rest
  | Lhs_star ->
      pp_string fmt "*"
  | Lhs_attr (recv, id) ->
      fprintf fmt "%a.%s" pp_expr recv id
  | Lhs_aref (recv, args) ->
      fprintf fmt "%a[%a]"
        pp_expr recv
        pp_arg_list args
  | Lhs_op (lhs, _)
  | Lhs_or lhs
  | Lhs_and lhs ->
      pp_lhs fmt lhs

and pp_do_block fmt { blk_vars = vars; blk_body = body } =
  pp_string fmt " do";
  if vars <> [] then
    fprintf fmt "|%a|" pp_lhs_list vars;
  fprintf fmt "@\n%a@]@\nend" pp_body body

and pp_brace_block fmt { blk_vars = vars; blk_body = body } =
  pp_string fmt " {";
  if vars <> [] then
    fprintf fmt "|%a|" pp_lhs_list vars;
  fprintf fmt "@\n%a@]@\n}@[<2>" pp_body body

and pp_lhs_list fmt = pp_list pp_lhs fmt

and pp_body_stmt fmt { body = body;
                       body_rescues = rescues;
                       body_else = elsbody;
                       body_ensure = ensbody } =
  fprintf fmt "%a@]" pp_body body;
  List.iter
    (fun (types, resbody) ->
       fprintf fmt "@\n@[<2>rescue";
       if types <> [] then
         fprintf fmt " %a" pp_arg_list types;
       fprintf fmt "@\n%a@]" pp_body resbody)
    rescues;
  if elsbody <> [] then
    fprintf fmt "@\n@[<2>else@\n%a@]@\n" pp_body elsbody;
  if ensbody <> [] then
    fprintf fmt "@\n@[<2>ensure@\n%a" pp_body ensbody;

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

and pp_binop fmt lhs op rhs =
  fprintf fmt "(@[%a %s %a@])"
    pp_expr lhs op pp_expr rhs

and pp_expr fmt = function
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

  | Variable (var, _) ->
    pp_string fmt (string_of_variable var)

  | Array (args, _) ->
      fprintf fmt "[@[%a]@]" pp_arg_list args

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
      fprintf fmt "@[<2>case %a@]@\n" (pp_opt pp_expr) expr;
      List.iter
        (fun (guards, whenbody) ->
           fprintf fmt "@[<2>when %a@\n%a@]@\n"
             pp_arg_list guards
             pp_body whenbody)
        whens;
      fprintf fmt "@[<2>else@\n%a@]@\n" pp_body elsbody;
      pp_string fmt "end"
  | Break (args, _) ->
      fprintf fmt "break %a" pp_arg_list args
  | Next (args, _) ->
      fprintf fmt "next %a" pp_arg_list args
  | Redo _ -> pp_string fmt "redo"
  | Retry _ -> pp_string fmt "retry"

  | Call (Some recv, id, args, blk, _) ->
      fprintf fmt "@[<2>";
      begin match id with
      | "|" | "^" | "&" | "<=>" | "=="
      | "===" | "=~" | ">" | ">=" | "<"
      | "<=" | "<<" | ">>" | "+" | "-"
      | "*" | "/" | "%" | "**" when List.length args = 1 ->
          begin match List.hd args with
          | Arg_value rhs -> pp_binop fmt recv id rhs
          | _ -> failwith "invalid binop"
          end
      | "~" | "+@" | "-@" when args = [] ->
          pp_char fmt id.[0];
          pp_expr fmt recv
      | "[]" ->
          fprintf fmt "%a[%a]"
            pp_expr recv
            pp_arg_list args
      | _ ->
          fprintf fmt "%a.%s(%a)"
            pp_expr recv
            id
            pp_arg_list args
      end;
      fprintf fmt "%a@]" (pp_opt pp_brace_block) blk
  | Call (None, id, args, blk, _) ->
      fprintf fmt "@[<2>%s(%a)%a@]"
        id
        pp_arg_list args
        (pp_opt pp_brace_block) blk

  | Return (args, _) ->
      fprintf fmt "return %a" pp_arg_list args

  | Yield (args, _) ->
      fprintf fmt "yield %a" pp_arg_list args

  | Super (Some args, blk, _) ->
      fprintf fmt "@[super %a%a@]"
        pp_arg_list args
        (pp_opt pp_do_block) blk
  | Super (None, _, _) ->
      pp_string fmt "super"

  | Assign (lhs, Array (args, _), Asgn_svalue, _) ->
      fprintf fmt "%a = %a"
        pp_lhs lhs
        pp_arg_list args
  | Assign (lhs, expr, _, _) ->
      fprintf fmt "%a %s= %a"
        pp_lhs lhs
        (match lhs with
         | Lhs_op (_, op) -> op
         | Lhs_or _       -> "||"
         | Lhs_and _      -> "&&"
         | _              -> "")
        pp_expr expr

  | Class (cpath, Some super, body, _) ->
      fprintf fmt "@[<2>class %a < %a@\n%a@]@\nend"
        pp_cpath cpath
        pp_expr super
        pp_body_stmt body
  | Class (cpath, None, body, _) ->
      fprintf fmt "@[<2>class %a@\n%a@]@\nend"
        pp_cpath cpath
        pp_body_stmt body

  | Sclass (recv, body, _) ->
      fprintf fmt "@[<2>class << %a@\n%a@]@\nend"
        pp_expr recv
        pp_body_stmt body

  | Module (cpath, body, _) ->
      fprintf fmt "@[<2>module %a@\n%a@]@\nend"
        pp_cpath cpath
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
