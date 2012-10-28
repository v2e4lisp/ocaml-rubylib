module type Annotation = sig
  type t
  val of_pos : Lexing.position -> t
end

module Position = struct
  type t = Lexing.position
  let of_pos pos = pos
end

module Generic = struct
  type 'a rb_literal =
    | Lit_string of 'a rb_string_contents list
    | Lit_xstring of 'a rb_string_contents list
    | Lit_symbol of 'a rb_string_contents list
    | Lit_integer of int
    | Lit_float of float
    | Lit_regexp of 'a rb_string_contents list * rb_regexp_flag

  and 'a rb_string_contents =
    | Str_contents of string
    | Str_interpol of 'a rb_expr

  and rb_regexp_flag =
    | Reg_none
    | Reg_once

  and 'a rb_variable =
    | Var_local of string
    | Var_dynamic of string
    | Var_instance of string
    | Var_class of string
    | Var_global of string
    | Var_const of 'a rb_cpath
    | Var_pseudo of rb_pseudo_variable

  and 'a rb_cpath =
    | Cpath_name of string
    | Cpath_relative of 'a rb_expr * string
    | Cpath_absolute of 'a rb_cpath

  and rb_pseudo_variable =
    | Pvar_nil
    | Pvar_true
    | Pvar_false
    | Pvar_self

  and 'a rb_parameter =
    | Param_req of string
    | Param_opt of string * 'a rb_expr
    | Param_rest of string
    | Param_star
    | Param_block of string

  and 'a rb_argument =
    | Arg_value of 'a rb_expr
    | Arg_splat of 'a rb_expr
    | Arg_block of 'a rb_expr
    | Arg_hash of ('a rb_expr * 'a rb_expr) list

  and 'a rb_lhs =
    | Lhs_var of 'a rb_variable
    | Lhs_decl of 'a rb_variable
    | Lhs_dstr of 'a rb_lhs list
    | Lhs_rest of 'a rb_lhs
    | Lhs_star
    | Lhs_attr of 'a rb_expr * string
    | Lhs_aref of 'a rb_expr * 'a rb_argument list

  and 'a rb_lhs_op =
    | Lhs_op of 'a rb_lhs * string
    | Lhs_or of 'a rb_lhs
    | Lhs_and of 'a rb_lhs

  and 'a rb_block = {
    blk_vars : 'a rb_lhs list;
    blk_body : 'a rb_stmt list
  }

  and 'a rb_case_stmt = {
    case_expr : 'a rb_expr option;
    case_whens : ('a rb_argument list * 'a rb_stmt list) list;
    case_else : 'a rb_stmt list;
  }

  and 'a rb_body_stmt = {
    body : 'a rb_stmt list;
    body_rescues : ('a rb_argument list * 'a rb_stmt list) list;
    body_else : 'a rb_stmt list;
    body_ensure : 'a rb_stmt list
  }

  and rb_assign_kind =
    | Asgn_single
    | Asgn_svalue
    | Asgn_multi

  and 'a rb_stmt =
    | Alias of string * string * 'a
    | Undef of string list * 'a

    | If_mod of 'a rb_stmt * 'a rb_expr * 'a
    | Unless_mod of 'a rb_stmt * 'a rb_expr * 'a
    | While_mod of 'a rb_stmt * 'a rb_expr * 'a
    | Until_mod of 'a rb_stmt * 'a rb_expr * 'a
    | Rescue_mod of 'a rb_stmt * 'a rb_stmt * 'a

    | Pre_exec of 'a rb_stmt list * 'a
    | Post_exec of 'a rb_stmt list * 'a

    | Expr of 'a rb_expr * 'a

  and 'a rb_expr =
    | Literal of 'a rb_literal * 'a
    | Variable of 'a rb_variable * 'a

    | Array of 'a rb_argument list * 'a
    | Hash of ('a rb_expr * 'a rb_expr) list * 'a
    | Dot2 of 'a rb_expr * 'a rb_expr * 'a
    | Dot3 of 'a rb_expr * 'a rb_expr * 'a

    | Not of 'a rb_expr * 'a
    | And of 'a rb_expr * 'a rb_expr * 'a
    | Or of 'a rb_expr * 'a rb_expr * 'a

    | Defined of 'a rb_expr * 'a

    | Ternary of 'a rb_expr * 'a rb_expr * 'a rb_expr * 'a
    | If of 'a rb_expr * 'a rb_stmt list * 'a rb_stmt list * 'a
    | Unless of 'a rb_expr * 'a rb_stmt list * 'a rb_stmt list * 'a
    | While of 'a rb_expr * 'a rb_stmt list * 'a
    | Until of 'a rb_expr * 'a rb_stmt list * 'a
    | For of 'a rb_lhs * 'a rb_expr * 'a rb_stmt list * 'a
    | Case of 'a rb_case_stmt * 'a
    | Break of 'a rb_argument list * 'a
    | Next of 'a rb_argument list * 'a
    | Redo of 'a
    | Retry of 'a

    | Call of 'a rb_expr option * string * 'a rb_argument list * 'a rb_block option * 'a
    | Return of 'a rb_argument list * 'a
    | Yield of 'a rb_argument list * 'a
    | Super of 'a rb_argument list option * 'a rb_block option * 'a

    | Assign of 'a rb_lhs * 'a rb_expr * rb_assign_kind * 'a
    | Op_assign of 'a rb_lhs_op * 'a rb_expr * 'a

    | Class of 'a rb_cpath * 'a rb_expr option * 'a rb_body_stmt * 'a
    | Sclass of 'a rb_expr * 'a rb_body_stmt * 'a
    | Module of 'a rb_cpath * 'a rb_body_stmt * 'a
    | Defn of string * 'a rb_parameter list * 'a rb_body_stmt * 'a
    | Defs of 'a rb_expr * string * 'a rb_parameter list * 'a rb_body_stmt * 'a

    | Begin of 'a rb_body_stmt * 'a
    | Seq of 'a rb_stmt list * 'a
end

module Make (A : Annotation) = struct
  include Generic
  type literal = A.t rb_literal
  type string_contents = A.t rb_string_contents
  type regexp_flag = rb_regexp_flag
  type variable = A.t rb_variable
  type cpath = A.t rb_cpath
  type pseudo_variable = rb_pseudo_variable
  type parameter = A.t rb_parameter
  type argument = A.t rb_argument
  type lhs = A.t rb_lhs
  type lhs_op = A.t rb_lhs_op
  type block = A.t rb_block
  type case_stmt = A.t rb_case_stmt
  type body_stmt = A.t rb_body_stmt
  type assign_kind = rb_assign_kind
  type stmt = A.t rb_stmt
  type expr = A.t rb_expr
end

include Make (Position)

let rec string_of_variable = function
  | Var_local (id)        -> id
  | Var_dynamic (id)      -> id
  | Var_instance (id)     -> Printf.sprintf "@%s" id
  | Var_class (id)        -> Printf.sprintf "@@%s" id
  | Var_global (id)       -> Printf.sprintf "$%s" id
  | Var_const (cpath)     -> name_of_cpath cpath
  | Var_pseudo Pvar_nil   -> "nil"
  | Var_pseudo Pvar_true  -> "true"
  | Var_pseudo Pvar_false -> "false"
  | Var_pseudo Pvar_self  -> "self"

and name_of_cpath = function
  | Cpath_name name          -> name
  | Cpath_relative (_, name) -> name
  | Cpath_absolute cpath     -> name_of_cpath cpath

let annot_of_stmt = function
  | Alias (_, _, a)
  | Undef (_, a)
  | If_mod (_, _, a)
  | Unless_mod (_, _, a)
  | While_mod (_, _, a)
  | Until_mod (_, _, a)
  | Rescue_mod (_, _, a)
  | Pre_exec (_, a)
  | Post_exec (_, a)
  | Expr (_, a)
    -> a

let annot_of_expr = function
  | Seq (_, a)
  | Defined (_, a)
  | Literal (_, a)
  | Variable (_, a)
  | Array (_, a)
  | Hash (_, a)
  | Dot2 (_, _, a)
  | Dot3 (_, _, a)
  | Begin (_, a)
  | Not (_, a)
  | And (_, _, a)
  | Or (_, _, a)
  | Ternary (_, _, _, a)
  | If (_, _, _, a)
  | Unless (_, _, _, a)
  | While (_, _, a)
  | Until (_, _, a)
  | For (_, _, _, a)
  | Case (_, a)
  | Break (_, a)
  | Next (_, a)
  | Redo (a)
  | Retry (a)
  | Call (_, _, _, _, a)
  | Return (_, a)
  | Yield (_, a)
  | Super (_, _, a)
  | Assign (_, _, _, a)
  | Op_assign (_, _, a)
  | Class (_, _, _, a)
  | Sclass (_, _, a)
  | Module (_, _, a)
  | Defn (_, _, _, a)
  | Defs (_, _, _, _, a)
    -> a

type pos = Lexing.position

let dummy_pos = Lexing.dummy_pos
