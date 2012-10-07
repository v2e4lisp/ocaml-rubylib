open Token
open Lexer_state

type t =
  | K_class | K_module | K_def | K_undef | K_begin | K_rescue | K_ensure | K_end | K_if | K_unless
  | K_then | K_elsif | K_else | K_case | K_when | K_while | K_until | K_for | K_break | K_next
  | K_redo | K_retry | K_in | K_do | K_do_cond | K_do_block | K_return | K_yield | K_super
  | K_self | K_nil | K_true | K_false | K_and | K_or | K_not | K_if_mod | K_unless_mod | K_while_mod
  | K_until_mod | K_rescue_mod | K_alias | K_defined | K_lbegin | K_lend | K__line__
  | K__file__

type lex_info = t * t * lex_state

let lex_info_exn = function
  | "end"      -> K_end,      K_end,         Expr_end
  | "else"     -> K_else,     K_else,        Expr_beg
  | "case"     -> K_case,     K_case,        Expr_beg
  | "ensure"   -> K_ensure,   K_ensure,      Expr_beg
  | "module"   -> K_module,   K_module,      Expr_beg
  | "elsif"    -> K_elsif,    K_elsif,       Expr_beg
  | "def"      -> K_def,      K_def,         Expr_fname
  | "rescue"   -> K_rescue,   K_rescue_mod,  Expr_mid
  | "not"      -> K_not,      K_not,         Expr_beg
  | "then"     -> K_then,     K_then,        Expr_beg
  | "yield"    -> K_yield,    K_yield,       Expr_arg
  | "for"      -> K_for,      K_for,         Expr_beg
  | "self"     -> K_self,     K_self,        Expr_end
  | "false"    -> K_false,    K_false,       Expr_end
  | "retry"    -> K_retry,    K_retry,       Expr_end
  | "return"   -> K_return,   K_return,      Expr_mid
  | "true"     -> K_true,     K_true,        Expr_end
  | "if"       -> K_if,       K_if_mod,      Expr_beg
  | "defined?" -> K_defined,  K_defined,     Expr_arg
  | "super"    -> K_super,    K_super,       Expr_arg
  | "undef"    -> K_undef,    K_undef,       Expr_fname
  | "break"    -> K_break,    K_break,       Expr_mid
  | "in"       -> K_in,       K_in,          Expr_beg
  | "do"       -> K_do,       K_do,          Expr_beg
  | "nil"      -> K_nil,      K_nil,         Expr_end
  | "until"    -> K_until,    K_until_mod,   Expr_beg
  | "unless"   -> K_unless,   K_unless_mod,  Expr_beg
  | "or"       -> K_or,       K_or,          Expr_beg
  | "next"     -> K_next,     K_next,        Expr_mid
  | "when"     -> K_when,     K_when,        Expr_beg
  | "redo"     -> K_redo,     K_redo,        Expr_end
  | "and"      -> K_and,      K_and,         Expr_beg
  | "begin"    -> K_begin,    K_begin,       Expr_beg
  | "__LINE__" -> K__line__,  K__line__,     Expr_end
  | "class"    -> K_class,    K_class,       Expr_class
  | "__FILE__" -> K__file__,  K__file__,     Expr_end
  | "END"      -> K_lend,     K_lend,        Expr_end
  | "BEGIN"    -> K_lbegin,   K_lbegin,      Expr_end
  | "while"    -> K_while,    K_while_mod,   Expr_beg
  | "alias"    -> K_alias,    K_alias,       Expr_fname
  | _          -> raise Not_found

let lex_info id =
  try Some (lex_info_exn id)
  with _ -> None

let mk_token ?(pos=Lexing.dummy_pos) = function
  | K_class      -> K_CLASS pos
  | K_module     -> K_MODULE pos
  | K_def        -> K_DEF pos
  | K_undef      -> K_UNDEF pos
  | K_begin      -> K_BEGIN pos
  | K_rescue     -> K_RESCUE pos
  | K_ensure     -> K_ENSURE pos
  | K_end        -> K_END pos
  | K_if         -> K_IF pos
  | K_unless     -> K_UNLESS pos
  | K_then       -> K_THEN pos
  | K_elsif      -> K_ELSIF pos
  | K_else       -> K_ELSE pos
  | K_case       -> K_CASE pos
  | K_when       -> K_WHEN pos
  | K_while      -> K_WHILE pos
  | K_until      -> K_UNTIL pos
  | K_for        -> K_FOR pos
  | K_break      -> K_BREAK pos
  | K_next       -> K_NEXT pos
  | K_redo       -> K_REDO pos
  | K_retry      -> K_RETRY pos
  | K_in         -> K_IN pos
  | K_do         -> K_DO pos
  | K_do_cond    -> K_DO_COND pos
  | K_do_block   -> K_DO_BLOCK pos
  | K_return     -> K_RETURN pos
  | K_yield      -> K_YIELD pos
  | K_super      -> K_SUPER pos
  | K_self       -> K_SELF pos
  | K_nil        -> K_NIL pos
  | K_true       -> K_TRUE pos
  | K_false      -> K_FALSE pos
  | K_and        -> K_AND pos
  | K_or         -> K_OR pos
  | K_not        -> K_NOT pos
  | K_if_mod     -> K_IF_MOD pos
  | K_unless_mod -> K_UNLESS_MOD pos
  | K_while_mod  -> K_WHILE_MOD pos
  | K_until_mod  -> K_UNTIL_MOD pos
  | K_rescue_mod -> K_RESCUE_MOD pos
  | K_alias      -> K_ALIAS pos
  | K_defined    -> K_DEFINED pos
  | K_lbegin     -> K_lBEGIN pos
  | K_lend       -> K_lEND pos
  | K__line__    -> K__LINE__ pos
  | K__file__    -> K__FILE__ pos
