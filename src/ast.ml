type pos = Lexing.position

let dummy_pos = Lexing.dummy_pos

type identifier = string

and literal =
  | Lit_string of string
  | Lit_symbol of string
  | Lit_int of int
  | Lit_float of float
  | Lit_regexp of string

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
  | Empty of 'a

  | Alias of identifier * identifier * 'a
  | Undef of identifier list * 'a
  | Defined of 'a expr * 'a

  | Nil of 'a
  | True of 'a
  | False of 'a
  | Self of 'a

  | Lit of literal * 'a
  | Str of string * 'a
  | Dstr of 'a expr list * 'a
  | Evstr of 'a expr * 'a
  | Xstr of string * 'a
  | Dxstr of 'a expr list * 'a
  | Dregx of 'a
  | Dregx_once of 'a
  | Nth_ref of int * 'a
  | Back_ref of char * 'a

  | Array of 'a expr list * 'a
  | Splat of 'a expr * 'a
  | Svalue of 'a expr list * 'a
  | Hash of 'a expr list * 'a
  | Dot2 of 'a expr * 'a expr * 'a
  | Dot3 of 'a expr * 'a expr * 'a

  | Preexec of 'a expr * 'a
  | Postexec of 'a expr * 'a
  | Block of 'a expr list * 'a
  | Begin of 'a begin_body * 'a

  | Not of 'a expr * 'a
  | And of 'a expr * 'a expr * 'a
  | Or of 'a expr * 'a expr * 'a
  | Match2 of 'a expr * 'a expr * 'a
  | Match3 of 'a expr * 'a expr * 'a
  | Match of 'a expr * 'a
  | Flip2 of 'a expr * 'a expr * 'a
  | Flip3 of 'a expr * 'a expr * 'a

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
  | Lvar of identifier * 'a
  | Dvar of identifier * 'a
  | Dsym of 'a expr list * 'a
  | Ivar of identifier * 'a
  | Cvar of identifier * 'a
  | Gvar of identifier * 'a

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

let is_empty = function
  | Empty _ -> true
  | _       -> false

let annot_of_expr = function
  | Empty (a)
  | Alias (_, _, a)
  | Undef (_, a)
  | Defined (_, a)
  | Nil (a)
  | True (a)
  | False (a)
  | Self (a)
  | Lit (_, a)
  | Str (_, a)
  | Dstr (_, a)
  | Evstr (_, a)
  | Xstr (_, a)
  | Dxstr (_, a)
  | Dregx (a)
  | Dregx_once (a)
  | Nth_ref (_, a)
  | Back_ref (_, a)
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
  | Match2 (_, _, a)
  | Match3 (_, _, a)
  | Match (_, a)
  | Flip2 (_, _, a)
  | Flip3 (_, _, a)
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
  | Dsym (_, a)
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
