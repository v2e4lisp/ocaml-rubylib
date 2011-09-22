type pos = Lexing.position

let dummy_pos = Lexing.dummy_pos

type identifier = string

and 'a literal =
  | Lit_string of 'a string_contents list
  | Lit_xstring of 'a string_contents list
  | Lit_symbol of 'a string_contents list
  | Lit_integer of int
  | Lit_float of float
  | Lit_regexp of 'a string_contents list * bool

and 'a string_contents =
  | Str_contents of string
  | Str_interpol of 'a expr

and 'a formal_param =
  | Param_id of identifier
  | Param_opt of identifier * 'a expr
  | Param_rest of identifier
  | Param_star
  | Param_block of identifier

and 'a begin_body = {
  body : 'a expr;
  body_rescues : ('a expr list * 'a expr) list;
  body_else : 'a expr;
  body_ensure : 'a expr;
}

and 'a case_body = {
  case_expr : 'a expr;
  case_whens : ('a expr list * 'a expr) list;
  case_else : 'a expr;
}

and 'a expr =
  | Empty

  (* Literals *)
  | Literal of 'a literal * 'a

  (* Local, Dynamic, Instance, Class, Global variables *)
  | Lvar of identifier * 'a
  | Dvar of identifier * 'a
  | Ivar of identifier * 'a
  | Cvar of identifier * 'a
  | Gvar of identifier * 'a

  (* Pseudo variables *)
  | Nil   of 'a
  | True  of 'a
  | False of 'a
  | Self  of 'a

  (* Array contructor: [...] *)
  | Array of 'a expr list * 'a

  (* Hash constructor: {...} *)
  | Hash of 'a expr list * 'a

  (* Range constructor: .., ... *)
  | Dot2 of 'a expr * 'a expr * 'a
  | Dot3 of 'a expr * 'a expr * 'a

  (* Logical expressions: not, and, or *)
  | Not of 'a expr * 'a
  | And of 'a expr * 'a expr * 'a
  | Or of 'a expr * 'a expr * 'a

  | Alias of identifier * identifier * 'a
  | Undef of identifier list * 'a
  | Defined of 'a expr * 'a

  | Splat of 'a expr * 'a
  | Svalue of 'a expr list * 'a

  | Preexec of 'a expr * 'a
  | Postexec of 'a expr * 'a

  | Block of 'a expr list * 'a
  | Begin of 'a begin_body * 'a

  | If of 'a expr * 'a expr * 'a expr * 'a
  | While of 'a expr * 'a expr * bool * 'a
  | Until of 'a expr * 'a expr * bool * 'a
  | For of 'a expr * 'a expr list * 'a expr * 'a
  | Case of 'a case_body * 'a
  | Break of 'a expr list * 'a
  | Next of 'a expr list * 'a
  | Redo of 'a
  | Retry of 'a

  | Call of 'a expr * identifier * 'a expr list * 'a
  | Iter of 'a expr * 'a expr list * 'a expr * 'a
  | Block_pass of 'a expr * 'a
  | Return of 'a expr list * 'a
  | Yield of 'a expr list * 'a
  | Super of 'a expr list * 'a
  | Zsuper of 'a

  | Const of identifier * 'a
  | Colon2 of 'a expr * identifier * 'a
  | Colon3 of identifier * 'a

  | Cdecl of identifier * 'a expr * 'a
  | Lasgn of identifier * 'a expr * 'a
  | Dasgn of identifier * 'a expr * 'a
  | Iasgn of identifier * 'a expr * 'a
  | Cvasgn of identifier * 'a expr * 'a
  | Cvdecl of identifier * 'a expr * 'a
  | Gasgn of identifier * 'a expr * 'a
  | Masgn of 'a expr * 'a expr * 'a

  | Op_asgn1 of 'a expr * 'a expr list * identifier * 'a expr * 'a
  | Op_asgn2 of 'a expr * identifier * identifier * 'a expr * 'a
  | Op_asgn of 'a expr * 'a expr * identifier * identifier  * 'a
  | Op_asgn_or of 'a expr * 'a expr * 'a
  | Op_asgn_and of 'a expr * 'a expr * 'a
  | Attrasgn of 'a expr * identifier * 'a expr list * 'a

  | Class of 'a expr * 'a expr * 'a expr * 'a
  | Sclass of 'a expr * 'a expr * 'a
  | Module of 'a expr * 'a expr * 'a
  | Defn of identifier * 'a formal_param list * 'a expr * 'a
  | Defs of 'a expr * identifier * 'a formal_param list * 'a expr * 'a

let annot_of_expr = function
  | Empty -> raise Not_found
  | Alias (_, _, a)
  | Undef (_, a)
  | Defined (_, a)
  | Nil (a)
  | True (a)
  | False (a)
  | Self (a)
  | Literal (_, a)
  | Array (_, a)
  | Splat (_, a)
  | Svalue (_, a)
  | Hash (_, a)
  | Dot2 (_, _, a)
  | Dot3 (_, _, a)
  | Preexec (_, a)
  | Postexec (_, a)
  | Block (_, a)
  | Begin (_, a)
  | Not (_, a)
  | And (_, _, a)
  | Or (_, _, a)
  | If (_, _, _, a)
  | While (_, _, _, a)
  | Until (_, _, _, a)
  | For (_, _, _, a)
  | Case (_, a)
  | Break (_, a)
  | Next (_, a)
  | Redo (a)
  | Retry (a)
  | Call (_, _, _, a)
  | Iter (_, _, _, a)
  | Block_pass (_, a)
  | Return (_, a)
  | Yield (_, a)
  | Super (_, a)
  | Zsuper (a)
  | Const (_, a)
  | Colon2 (_, _, a)
  | Colon3 (_, a)
  | Lvar (_, a)
  | Dvar (_, a)
  | Ivar (_, a)
  | Cvar (_, a)
  | Gvar (_, a)
  | Cdecl (_, _, a)
  | Lasgn (_, _, a)
  | Dasgn (_, _, a)
  | Iasgn (_, _, a)
  | Cvasgn (_, _, a)
  | Cvdecl (_, _, a)
  | Gasgn (_, _, a)
  | Masgn (_, _, a)
  | Op_asgn1 (_, _, _, _, a)
  | Op_asgn2 (_, _, _, _, a)
  | Op_asgn (_, _, _, _, a)
  | Op_asgn_or (_, _, a)
  | Op_asgn_and (_, _, a)
  | Attrasgn (_, _, _, a)
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
