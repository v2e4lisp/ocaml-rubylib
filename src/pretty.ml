open Format
open Ast

let pp_print_expr fmt expr =
  let rec pp_fixme () = pp_string "TODO"
  and pp_char c = pp_string (String.make 1 c)
  and pp_string = pp_print_as fmt 0
  and pp_int int = pp_string (string_of_int int)
  and pp_float float = pp_string (string_of_float float)
  and pp_space () =
    pp_char ' ';
    pp_cut ()
  and pp_cut = pp_print_cut fmt
  and pp_newline = pp_force_newline fmt
  and pp_open ?(n=2) () = pp_open_box fmt n
  and pp_close = pp_close_box fmt
  and pp_string_lit s =
    (* TODO *)
    pp_char '"';
    pp_string (String.escaped s);
    pp_char '"'
  and pp_symbol s =
    pp_char ':';
    pp_string s
  and pp_paren f =
    pp_char '(';
    pp_open ~n:1 ();
    f ();
    pp_close ();
    pp_char ')'
  and pp_binop lhs op rhs =
    pp_paren
      (fun () ->
         pp_expr lhs;
         pp_char ' ';
         pp_string op;
         pp_open ();
         pp_space ();
         pp_expr rhs;
         pp_close ())
  and pp_bare_array = function
    | [] -> ()
    | [e] -> pp_expr e;
    | e :: es ->
        pp_expr e;
        pp_char ',';
        pp_space ();
        pp_bare_array es
  and pp_bare_hash = function
    | [] -> ()
    | k :: v :: es ->
        pp_expr k;
        pp_string " =>";
        pp_space ();
        pp_expr v;
        if es <> [] then begin
          pp_char ',';
          pp_space ()
        end;
        pp_bare_hash es
    | _ ->
        pp_fixme ()
  and pp_array es =
    pp_char '[';
    pp_open ();
    pp_cut ();
    pp_bare_array es;
    pp_close ();
    pp_cut ();
    pp_char ']'
  and pp_args args =
    pp_paren
      (fun () ->
         pp_bare_array args)
  and pp_bare_args = function
    | [] -> ()
    | args ->        
        pp_open ();
        pp_bare_array args;
        pp_close ()
  and pp_param = function
    | Param_id id ->
        pp_string id
    | Param_opt (_, asgn) ->
        pp_expr asgn
    | Param_rest id ->
        pp_char '*';
        pp_string id
    | Param_star ->
        pp_char '*'
    | Param_block id ->
        pp_char '&';
        pp_string id
  and pp_bare_params = function
    | [] -> ()
    | [p] -> pp_param p
    | p :: ps ->
        pp_param p;
        pp_char ',';
        pp_space ();
        pp_bare_params ps
  and pp_params params =
    pp_char '(';
    pp_open ();
    pp_bare_params params;
    pp_close ();
    pp_char ')'
  and pp_stmt stmt = pp_expr ~stmt:true stmt
  and pp_stmts = function
    | [] -> ()
    | [s] -> pp_expr s;
    | s :: ss ->
        pp_stmt s;
        pp_newline ();
        pp_stmts ss
  and pp_expr ?(stmt=false) = function
    | Empty ->
        if not stmt then
          pp_string "()"

    | Alias (new_id, old_id, _) ->
        pp_string "alias";
        pp_open ();
        pp_space ();
        pp_string new_id;
        pp_space ();
        pp_string old_id;
        pp_close ()

    | Undef (ids, _) ->
        pp_string "undef";
        pp_open ();
        List.iter
          (fun id ->
             pp_space ();
             pp_string id)
          ids;
        pp_close ()

    | Defined (e, _) ->
        pp_string "defined";
        pp_open ();
        pp_char ' ';
        pp_expr e;
        pp_close ()

    | Nil _ -> pp_string "nil"
    | True _ -> pp_string "true"
    | False _ -> pp_string "false"
    | Self _ -> pp_string "self"

    | Lit (Lit_string string, _) ->
        pp_string_lit string
    | Lit (Lit_symbol symbol, _) ->
        pp_symbol symbol
    | Lit (Lit_int int, _) ->
        pp_int int
    | Lit (Lit_float float, _) ->
        pp_float float
    | Lit (Lit_regexp regexp, _) ->
        pp_char '/';
        pp_string regexp;
        pp_char '/'

    | Str (s, _) ->
        pp_string_lit s

    | Dstr (es, _) ->
        pp_fixme ()

    | Evstr (e, _) ->
        pp_string "#{";
        pp_expr e;
        pp_char '}'

    | Xstr (s, _) ->
        pp_fixme ()

    | Dxstr (es, _) ->
        pp_fixme ()

    | Dregx _ ->
        pp_fixme ()

    | Dregx_once _ ->
        pp_fixme ()

    | Nth_ref (n, _) ->
        pp_char '$';
        pp_int n

    | Back_ref (c, _) ->
        pp_char '$';
        pp_char c

    | Array (es, _) ->
        pp_array es

    | Splat (e, _) ->
        pp_char '*';
        pp_expr e

    | Svalue (es, _) ->
        pp_bare_array es

    | Hash (hash, _) ->
        pp_char '{';
        pp_open ();
        pp_space ();
        pp_bare_hash hash;
        pp_space ();
        pp_close ();
        pp_char '}'

    | Dot2 (e1, e2, _) ->
        pp_binop e1 ".." e2

    | Dot3 (e1, e2, _) ->
        pp_binop e1 "..." e2

    | Preexec (e, _) ->
        pp_string "BEGIN";
        pp_string " {";
        pp_open ();
        pp_space ();
        pp_stmt e;
        pp_space ();
        pp_close ();
        pp_char '}'
                     
    | Postexec (e, _) ->
        pp_string "END";
        pp_string " {";
        pp_open ();
        pp_space ();
        pp_stmt e;
        pp_space ();
        pp_close ();
        pp_char '}'

    | Block (stmts, _) ->
        if stmt then
          pp_stmts stmts
        else
          pp_paren
            (fun () ->
               pp_stmts stmts)

    | Begin ({ body = e;
               body_rescues = res;
               body_else = els;
               body_ensure = ens }, _) ->
        pp_string "begin";
        pp_newline ();
        pp_open ();
        pp_stmt e;
        pp_close ();
        pp_newline ();
        List.iter
          (fun (types, e) ->
             pp_string "rescue";
             if types <> [] then begin
               pp_char ' ';
               pp_bare_array types
             end;
             pp_open ();
             pp_newline ();
             pp_stmt e;
             pp_close ();
             pp_newline ())
          res;
        if els <> Empty then begin
          pp_string "else";
          pp_newline ();
          pp_open ();
          pp_stmt e;
          pp_close ();
          pp_newline ()
        end;
        if els <> Empty then begin
          pp_string "ensure";
          pp_newline ();
          pp_open ();
          pp_stmt e;
          pp_close ();
          pp_newline ()
        end;
        pp_string "end";

    | Not (e, _) ->
        pp_string "(not";
        pp_open ();
        pp_space ();
        pp_expr e;
        pp_close ();
        pp_char ')'

    | And (e1, e2, _) ->
        pp_binop e1 "&&" e2

    | Or (e1, e2, _) ->
        pp_binop e1 "||" e2

    | Match2 (e1, e2, _) ->
        pp_binop e1 "=~" e2

    | Match3 (e1, e2, _) ->
        pp_binop e2 "=~" e1

    | Match (e, _) ->
        pp_expr e

    | Flip2 (e1, e2, _) ->
        pp_binop e1 ".." e2

    | Flip3 (e1, e2, _) ->
        pp_binop e1 "..." e2

    | If (c, t, e, _) ->
        pp_string "if ";
        pp_expr c;
        pp_open ();
        pp_newline ();
        pp_stmt t;
        pp_close ();
        pp_newline ();
        if e <> Empty then begin
          pp_string "else";
          pp_open ();
          pp_newline ();
          pp_stmt e;
          pp_close ();
          pp_newline ()
        end;
        pp_string "end"

    | While (c, e, pre, _) ->
        if pre then begin
          pp_expr e;
          pp_space ();
          pp_string "while";
          pp_open ();
          pp_space ();
          pp_expr c;
          pp_close ()
        end else begin
          pp_string "while";
          pp_open ();
          pp_space ();
          pp_expr c;
          pp_newline ();
          pp_stmt e;
          pp_close ();
          pp_space ();
          pp_string "end"
        end

    | Until (c, e, pre, _) ->
        if pre then begin
          pp_expr e;
          pp_space ();
          pp_string "until";
          pp_open ();
          pp_space ();
          pp_expr c;
          pp_close ()
        end else begin
          pp_string "until";
          pp_open ();
          pp_space ();
          pp_expr c;
          pp_newline ();
          pp_stmt e;
          pp_close ();
          pp_space ();
          pp_string "end"
        end

    | For (gen, vars, e, _) ->
        pp_fixme ()

    | Case ({ case_expr = e;
              case_whens = whens;
              case_else = els }, _) ->
        pp_string "case";
        pp_char ' ';
        pp_expr e;
        pp_newline ();
        List.iter
          (fun (guards, e) ->
             pp_string "when";
             pp_char ' ';
             pp_bare_array guards;
             pp_open ();
             pp_newline ();
             pp_stmt e;
             pp_close ();
             pp_newline ())
          whens;
        if els <> Empty then begin
          pp_string "else";
          pp_open ();
          pp_newline ();
          pp_stmt els;
          pp_close ();
          pp_newline ()
        end;
        pp_string "end"

    | Break (args, _) ->
        pp_string "break ";
        pp_bare_args args

    | Next (args, _) ->
        pp_string "next ";
        pp_bare_args args

    | Redo _ -> pp_string "redo"
    | Retry _ -> pp_string "retry"

    | Call (recv, id, args, _) ->
        (match id with
         | "|" | "^" | "&" | "<=>" | "=="
         | "===" | "=~" | ">" | ">=" | "<"
         | "<=" | "<<" | ">>" | "+" | "-"
         | "*" | "/" | "%" | "**" when List.length args = 1 ->
             pp_binop recv id (List.hd args)
         | "~" | "+@" | "-@" when args = [] ->
             pp_char id.[0];
             pp_expr recv
         | "[]" ->
             pp_expr recv;
             pp_char '[';
             pp_bare_args args;
             pp_char ']';
         | _ ->
             if recv <> Empty then begin
               pp_expr recv;
               pp_char '.'
             end;
             pp_string id;
             if args <> [] then
               pp_args args)

    | Iter (call, args, e, _) ->
        pp_expr call;
        pp_string " {";
        pp_open ();
        if args <> [] then begin
          pp_char '|';
          pp_bare_array args;
          pp_char '|'
        end;
        pp_newline ();
        pp_stmt e;
        pp_close ();
        pp_newline ();
        pp_char '}'

    | Block_pass (e, _) ->
        pp_char '&';
        pp_expr e

    | Return (args, _) ->
        pp_string "return ";
        pp_bare_args args

    | Yield (args, _) ->
        pp_string "yield";
        pp_args args

    | Super (args, _) ->
        pp_string "super";
        pp_args args

    | Zsuper _ ->
        pp_string "super"

    | Const (id, _) ->
        pp_string id

    | Colon2 (path, id, _) ->
        if path <> Empty then begin
          pp_expr path;
          pp_string "::";
        end;
        pp_string id

    | Colon3 (id, _) ->
        pp_string "::";
        pp_string id

    | Lvar (id, _)
    | Dvar (id, _) ->
        pp_string id
    | Dsym (es, _) ->
        pp_fixme ()
    | Ivar (id, _)
    | Cvar (id, _)
    | Gvar (id, _) ->
        pp_string id

    | Cdecl (id, e, _)
    | Lasgn (id, e, _)
    | Dasgn (id, e, _)
    | Iasgn (id, e, _)
    | Cvasgn (id, e, _)
    | Cvdecl (id, e, _)
    | Gasgn (id, e, _) ->
        pp_string id;
        if e <> Empty then begin
          (* TODO *)
          pp_string " =";
          pp_open ();
          pp_space ();
          pp_expr e;
          pp_close ()
        end

    | Masgn (lhs, rhs, _) ->
        pp_fixme ()

    | Op_asgn1 (recv, args, op, e, _) ->
        pp_expr recv;
        pp_char '[';
        pp_open ();
        pp_bare_args args;
        pp_close ();
        pp_string "] ";
        pp_string op;
        pp_char '=';
        pp_open ();
        pp_space ();
        pp_expr e;
        pp_close ()

    | Op_asgn2 (recv, id, op, e, _) ->
        pp_expr recv;
        pp_char '.';
        pp_string id;
        pp_char ' ';
        pp_string op;
        pp_char '=';
        pp_open ();
        pp_space ();
        pp_expr e;
        pp_close ()

    | Op_asgn (recv, e, id, op, _) ->
        pp_expr recv;
        pp_string "::";
        pp_string id;
        pp_char ' ';
        pp_string op;
        pp_char '=';
        pp_open ();
        pp_space ();
        pp_expr e;
        pp_close ()

    | Op_asgn_or (recv, e, _) ->
        pp_expr recv;
        pp_string " ||=";
        pp_open ();
        pp_space ();
        pp_expr e;
        pp_close ()

    | Op_asgn_and (recv, e, _) ->
        pp_expr recv;
        pp_string " &&=";
        pp_open ();
        pp_space ();
        pp_expr e;
        pp_close ()

    | Attrasgn (e, id, es, _) ->
        pp_fixme ()

    | Class (path, super, e, _) ->
        pp_string "class ";
        pp_open ();
        pp_expr path;
        if super <> Empty then begin
          pp_string " <";
          pp_space ();
          pp_expr super
        end;
        pp_newline ();
        pp_stmt e;
        pp_close ();
        pp_newline ();
        pp_string "end"

    | Sclass (recv, e, _) ->
        pp_string "class <<";
        pp_open ();
        pp_expr recv;
        pp_newline ();
        pp_stmt e;
        pp_close ();
        pp_newline ();
        pp_string "end"

    | Module (path, e, _) ->
        pp_string "module ";
        pp_expr path;
        pp_open ();
        pp_newline ();
        pp_stmt e;
        pp_close ();
        pp_newline ();
        pp_string "end"

    | Defn (id, params, e, _) ->
        pp_string "def ";
        pp_string id;
        pp_open ();
        pp_params params;
        pp_newline ();
        pp_stmt e;
        pp_close ();
        pp_newline ();
        pp_string "end"

    | Defs (recv, id, params, e, _) ->
        pp_string "def ";
        pp_expr recv;
        pp_char '.';
        pp_string id;
        pp_open ();
        pp_params params;
        pp_newline ();
        pp_stmt e;
        pp_close ();
        pp_newline ();
        pp_string "end"
  in
    pp_expr expr;
    pp_print_flush fmt ()

let print_expr expr =
  pp_print_expr std_formatter expr
