type pos = Lexing.position

let dummy_pos = Lexing.dummy_pos

type 'a literal =
  | Lit_string of 'a string_contents list
  | Lit_xstring of 'a string_contents list
  | Lit_symbol of 'a string_contents list
  | Lit_integer of int
  | Lit_float of float
  | Lit_regexp of 'a string_contents list * bool

and 'a string_contents =
  | Str_contents of string
  | Str_interpol of 'a expr

and 'a identifier =
  | Id_local of string
  | Id_dyn of string
  | Id_inst of string
  | Id_class of string
  | Id_glob of string
  | Id_const of 'a cpath
  | Id_pseudo of pseudo_variable

and 'a cpath =
  | Cpath_name of string
  | Cpath_rel of 'a expr * string
  | Cpath_glob of 'a cpath

and pseudo_variable =
  | Pid_nil
  | Pid_true
  | Pid_false
  | Pid_self

and 'a parameter =
  | Param_id of string
  | Param_opt of string * 'a expr
  | Param_rest of string
  | Param_star
  | Param_block of string

and 'a argument =
  | Arg_value of 'a expr
  | Arg_splat of 'a expr
  | Arg_block of 'a expr
  | Arg_hash of 'a expr list

and 'a lhs =
  | Lhs_id of 'a identifier
  | Lhs_decl of 'a identifier
  | Lhs_dstr of 'a lhs list
  | Lhs_rest of 'a lhs
  | Lhs_star
  | Lhs_attr of 'a expr * string
  | Lhs_aref of 'a expr * 'a argument list
  | Lhs_op of 'a lhs * string
  | Lhs_or of 'a lhs
  | Lhs_and of 'a lhs

and 'a case_stmt = {
  case_expr : 'a expr;
  case_whens : ('a argument list * 'a stmt list) list;
  case_else : 'a stmt list;
}

and 'a body_stmt = {
  body : 'a stmt list;
  body_rescues : ('a argument list * 'a stmt list) list;
  body_else : 'a stmt list;
  body_ensure : 'a stmt list
}

and 'a stmt =
  | Alias of string * string * 'a
  | Undef of string list * 'a

  | If_mod of 'a stmt * 'a expr * 'a
  | Unless_mod of 'a stmt * 'a expr * 'a
  | While_mod of 'a stmt * 'a expr * 'a
  | Until_mod of 'a stmt * 'a expr * 'a
  | Rescue_mod of 'a stmt * 'a stmt * 'a

  | Pre_exec of 'a stmt list * 'a
  | Post_exec of 'a stmt list * 'a

  | Expr of 'a expr * 'a

and 'a expr =
  | Empty

  | Literal of 'a literal * 'a
  | Identifier of 'a identifier * 'a

  | Array of 'a argument list * 'a
  | Hash of 'a expr list * 'a
  | Dot2 of 'a expr * 'a expr * 'a
  | Dot3 of 'a expr * 'a expr * 'a

  | Not of 'a expr * 'a
  | And of 'a expr * 'a expr * 'a
  | Or of 'a expr * 'a expr * 'a

  | Defined of 'a expr * 'a

  | Svalue of 'a argument list * 'a

  | Ternary of 'a expr * 'a expr * 'a expr * 'a
  | If of 'a expr * 'a stmt list * 'a stmt list * 'a
  | Unless of 'a expr * 'a stmt list * 'a stmt list * 'a
  | While of 'a expr * 'a stmt list * 'a
  | Until of 'a expr * 'a stmt list * 'a
  | For of 'a lhs * 'a expr * 'a stmt list * 'a
  | Case of 'a case_stmt * 'a
  | Break of 'a argument list * 'a
  | Next of 'a argument list * 'a
  | Redo of 'a
  | Retry of 'a

  | Call of 'a expr * string * 'a argument list * 'a
  | Iter of 'a expr * 'a lhs list * 'a stmt list * 'a
  | Return of 'a argument list * 'a
  | Yield of 'a argument list * 'a
  | Super of 'a argument list option * 'a

  | Assign of 'a lhs * 'a expr * 'a

  | Class of 'a cpath * 'a expr * 'a body_stmt * 'a
  | Sclass of 'a expr * 'a body_stmt * 'a
  | Module of 'a cpath * 'a body_stmt * 'a
  | Defn of string * 'a parameter list * 'a body_stmt * 'a
  | Defs of 'a expr * string * 'a parameter list * 'a body_stmt * 'a

  | Begin of 'a body_stmt * 'a
  | Block of 'a stmt list * 'a

let rec string_of_identifier = function
  | Id_local (id)       -> id
  | Id_dyn (id)         -> id
  | Id_inst (id)        -> Printf.sprintf "@%s" id
  | Id_class (id)       -> Printf.sprintf "@@%s" id
  | Id_glob (id)        -> Printf.sprintf "$%s" id
  | Id_const (cpath)    -> name_of_cpath cpath
  | Id_pseudo Pid_nil   -> "nil"
  | Id_pseudo Pid_true  -> "true"
  | Id_pseudo Pid_false -> "false"
  | Id_pseudo Pid_self  -> "self"

and name_of_cpath = function
  | Cpath_name name     -> name
  | Cpath_rel (_, name) -> name
  | Cpath_glob cpath    -> name_of_cpath cpath

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
  | Empty -> raise Not_found
  | Block (_, a)
  | Defined (_, a)
  | Literal (_, a)
  | Identifier (_, a)
  | Array (_, a)
  | Svalue (_, a)
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
  | Call (_, _, _, a)
  | Iter (_, _, _, a)
  | Return (_, a)
  | Yield (_, a)
  | Super (_, a)
  | Assign (_, _, a)
  | Class (_, _, _, a)
  | Sclass (_, _, a)
  | Module (_, _, a)
  | Defn (_, _, _, a)
  | Defs (_, _, _, _, a)
    -> a

module type Annot = sig
  type t
  val of_pos : Lexing.position -> t
end

module Pos : Annot = struct
  type t = Lexing.position
  let of_pos pos = pos
end
