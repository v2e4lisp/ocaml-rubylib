type pos = Lexing.position

type identifier = string

and literal =
  | Lit_string of string
  | Lit_symbol of string
  | Lit_int of int
  | Lit_float of float
  | Lit_regexp of string

and formal_param =
  | Param_id of identifier
  | Param_opt of identifier * expr
  | Param_rest of identifier
  | Param_star
  | Param_block of identifier

and begin_body = {
  body : expr;
  body_rescues : (expr list * expr) list;
  body_else : expr;
  body_ensure : expr;
}

and case_body = {
  case_expr : expr;
  case_whens : (expr list * expr) list;
  case_else : expr;
}

and expr =
  | Empty
  | Alias of identifier * identifier * pos
  | Undef of identifier list * pos
  | Defined of expr * pos
  | Nil of pos
  | True of pos
  | False of pos
  | Self of pos
  | Lit of literal * pos
  | Str of string * pos
  | Dstr of expr list * pos
  | Evstr of expr * pos
  | Xstr of string * pos
  | Dxstr of expr list * pos
  | Dregx of pos
  | Dregx_once of pos
  | Nth_ref of int * pos
  | Back_ref of char * pos
  | Array of expr list * pos
  | Splat of expr * pos
  | Svalue of expr list * pos
  | Hash of expr list * pos
  | Dot2 of expr * expr * pos
  | Dot3 of expr * expr * pos
  | Preexec of expr * pos
  | Postexec of expr * pos
  | Block of expr list * pos
  | Begin of begin_body * pos
  | Not of expr * pos
  | And of expr * expr * pos
  | Or of expr * expr * pos
  | Match2 of expr * expr * pos
  | Match3 of expr * expr * pos
  | Match of expr * pos
  | Flip2 of expr * expr * pos
  | Flip3 of expr * expr * pos
  | If of expr * expr * expr * pos
  | While of expr * expr * bool * pos
  | Until of expr * expr * bool * pos
  | For of expr * expr list * expr * pos
  | Case of case_body * pos
  | Break of expr list * pos
  | Next of expr list * pos
  | Redo of pos
  | Retry of pos
  | Call of expr * identifier * expr list * pos
  | Iter of expr * expr list * expr * pos
  | Block_pass of expr * pos
  | Return of expr list * pos
  | Yield of expr list * pos
  | Super of expr list * pos
  | Zsuper of pos
  | Const of identifier * pos
  | Colon2 of expr * identifier * pos
  | Colon3 of identifier * pos
  | Lvar of identifier * pos
  | Dvar of identifier * pos
  | Dsym of expr list * pos
  | Ivar of identifier * pos
  | Cvar of identifier * pos
  | Gvar of identifier * pos
  | Cdecl of identifier * expr * pos
  | Lasgn of identifier * expr * pos
  | Dasgn of identifier * expr * pos
  | Iasgn of identifier * expr * pos
  | Cvasgn of identifier * expr * pos
  | Cvdecl of identifier * expr * pos
  | Gasgn of identifier * expr * pos
  | Masgn of expr * expr * pos
  | Op_asgn1 of expr * expr list * identifier * expr * pos
  | Op_asgn2 of expr * identifier * identifier * expr * pos
  | Op_asgn of expr * expr * identifier * identifier  * pos
  | Op_asgn_or of expr * expr * pos
  | Op_asgn_and of expr * expr * pos
  | Attrasgn of expr * identifier * expr list * pos
  | Class of expr * expr * expr * pos
  | Sclass of expr * expr * pos
  | Module of expr * expr * pos
  | Defn of identifier * formal_param list * expr * pos
  | Defs of expr * identifier * formal_param list * expr * pos

(* need deriving-ocsigen
with show

module Show_pos = struct
  let show t = "<pos>"
  let format fmt t =
    Format.pp_print_string fmt
      (if t == Lexing.dummy_pos then
         "<pos>"
       else
         Printf.sprintf "%s:%d:%d"
           t.Lexing.pos_fname
           t.Lexing.pos_lnum
           (t.Lexing.pos_cnum - t.Lexing.pos_bol))
end

let string_of_expr = Show.show<expr>
*)

let pos_of_expr = function
  | Empty -> Lexing.dummy_pos
  | Alias (_, _, pos)
  | Undef (_, pos)
  | Defined (_, pos)
  | Nil (pos)
  | True (pos)
  | False (pos)
  | Self (pos)
  | Lit (_, pos)
  | Str (_, pos)
  | Dstr (_, pos)
  | Evstr (_, pos)
  | Xstr (_, pos)
  | Dxstr (_, pos)
  | Dregx (pos)
  | Dregx_once (pos)
  | Nth_ref (_, pos)
  | Back_ref (_, pos)
  | Array (_, pos)
  | Splat (_, pos)
  | Svalue (_, pos)
  | Hash (_, pos)
  | Dot2 (_, _, pos)
  | Dot3 (_, _, pos)
  | Preexec (_, pos)
  | Postexec (_, pos)
  | Block (_, pos)
  | Begin (_, pos)
  | Not (_, pos)
  | And (_, _, pos)
  | Or (_, _, pos)
  | Match2 (_, _, pos)
  | Match3 (_, _, pos)
  | Match (_, pos)
  | Flip2 (_, _, pos)
  | Flip3 (_, _, pos)
  | If (_, _, _, pos)
  | While (_, _, _, pos)
  | Until (_, _, _, pos)
  | For (_, _, _, pos)
  | Case (_, pos)
  | Break (_, pos)
  | Next (_, pos)
  | Redo (pos)
  | Retry (pos)
  | Call (_, _, _, pos)
  | Iter (_, _, _, pos)
  | Block_pass (_, pos)
  | Return (_, pos)
  | Yield (_, pos)
  | Super (_, pos)
  | Zsuper (pos)
  | Const (_, pos)
  | Colon2 (_, _, pos)
  | Colon3 (_, pos)
  | Lvar (_, pos)
  | Dvar (_, pos)
  | Dsym (_, pos)
  | Ivar (_, pos)
  | Cvar (_, pos)
  | Gvar (_, pos)
  | Cdecl (_, _, pos)
  | Lasgn (_, _, pos)
  | Dasgn (_, _, pos)
  | Iasgn (_, _, pos)
  | Cvasgn (_, _, pos)
  | Cvdecl (_, _, pos)
  | Gasgn (_, _, pos)
  | Masgn (_, _, pos)
  | Op_asgn1 (_, _, _, _, pos)
  | Op_asgn2 (_, _, _, _, pos)
  | Op_asgn (_, _, _, _, pos)
  | Op_asgn_or (_, _, pos)
  | Op_asgn_and (_, _, pos)
  | Attrasgn (_, _, _, pos)
  | Class (_, _, _, pos)
  | Sclass (_, _, pos)
  | Module (_, _, pos)
  | Defn (_, _, _, pos)
  | Defs (_, _, _, _, pos)
    -> pos
