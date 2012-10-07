type token =
  | K_CLASS of (Lexing.position)
  | K_MODULE of (Lexing.position)
  | K_DEF of (Lexing.position)
  | K_UNDEF of (Lexing.position)
  | K_BEGIN of (Lexing.position)
  | K_RESCUE of (Lexing.position)
  | K_ENSURE of (Lexing.position)
  | K_END of (Lexing.position)
  | K_IF of (Lexing.position)
  | K_UNLESS of (Lexing.position)
  | K_THEN of (Lexing.position)
  | K_ELSIF of (Lexing.position)
  | K_ELSE of (Lexing.position)
  | K_CASE of (Lexing.position)
  | K_WHEN of (Lexing.position)
  | K_WHILE of (Lexing.position)
  | K_UNTIL of (Lexing.position)
  | K_FOR of (Lexing.position)
  | K_BREAK of (Lexing.position)
  | K_NEXT of (Lexing.position)
  | K_REDO of (Lexing.position)
  | K_RETRY of (Lexing.position)
  | K_IN of (Lexing.position)
  | K_DO of (Lexing.position)
  | K_DO_COND of (Lexing.position)
  | K_DO_BLOCK of (Lexing.position)
  | K_RETURN of (Lexing.position)
  | K_YIELD of (Lexing.position)
  | K_SUPER of (Lexing.position)
  | K_SELF of (Lexing.position)
  | K_NIL of (Lexing.position)
  | K_TRUE of (Lexing.position)
  | K_FALSE of (Lexing.position)
  | K_AND of (Lexing.position)
  | K_OR of (Lexing.position)
  | K_NOT of (Lexing.position)
  | K_IF_MOD of (Lexing.position)
  | K_UNLESS_MOD of (Lexing.position)
  | K_WHILE_MOD of (Lexing.position)
  | K_UNTIL_MOD of (Lexing.position)
  | K_RESCUE_MOD of (Lexing.position)
  | K_ALIAS of (Lexing.position)
  | K_DEFINED of (Lexing.position)
  | K_lBEGIN of (Lexing.position)
  | K_lEND of (Lexing.position)
  | K__LINE__ of (Lexing.position)
  | K__FILE__ of (Lexing.position)
  | IDENTIFIER of (string * Lexing.position)
  | FID of (string * Lexing.position)
  | GVAR of (string * Lexing.position)
  | IVAR of (string * Lexing.position)
  | CONSTANT of (string * Lexing.position)
  | CVAR of (string * Lexing.position)
  | STRING_CONTENT of (string * Lexing.position)
  | INTEGER of (int * Lexing.position)
  | FLOAT of (float * Lexing.position)
  | REGEXP_END of (string * Lexing.position)
  | UPLUS of (Lexing.position)
  | UMINUS of (Lexing.position)
  | UMINUS_NUM of (Lexing.position)
  | POW of (Lexing.position)
  | CMP of (Lexing.position)
  | EQL of (Lexing.position)
  | EQ of (Lexing.position)
  | EQQ of (Lexing.position)
  | NEQ of (Lexing.position)
  | GEQ of (Lexing.position)
  | LEQ of (Lexing.position)
  | ANDOP of (Lexing.position)
  | OROP of (Lexing.position)
  | MATCH of (Lexing.position)
  | NMATCH of (Lexing.position)
  | DOT of (Lexing.position)
  | DOT2 of (Lexing.position)
  | DOT3 of (Lexing.position)
  | AREF of (Lexing.position)
  | ASET of (Lexing.position)
  | LSHFT of (Lexing.position)
  | RSHFT of (Lexing.position)
  | COLON2 of (Lexing.position)
  | COLON3 of (Lexing.position)
  | OP_ASGN of (string * Lexing.position)
  | ASSOC of (Lexing.position)
  | LPAREN of (Lexing.position)
  | LPAREN2 of (Lexing.position)
  | RPAREN of (Lexing.position)
  | LPAREN_ARG of (Lexing.position)
  | LB of (Lexing.position)
  | LBRACK of (Lexing.position)
  | RBRACK of (Lexing.position)
  | LBRACE of (Lexing.position)
  | LBRACE_ARG of (Lexing.position)
  | STAR of (Lexing.position)
  | STAR2 of (Lexing.position)
  | AMPER of (Lexing.position)
  | AMPER2 of (Lexing.position)
  | TILDE of (Lexing.position)
  | PERCENT of (Lexing.position)
  | DIVIDE of (Lexing.position)
  | PLUS of (Lexing.position)
  | MINUS of (Lexing.position)
  | LT of (Lexing.position)
  | GT of (Lexing.position)
  | PIPE of (Lexing.position)
  | BANG of (Lexing.position)
  | CARET of (Lexing.position)
  | LCURLY of (Lexing.position)
  | RCURLY of (Lexing.position)
  | BACK_REF2 of (Lexing.position)
  | SYMBEG of (Lexing.position)
  | STRING_BEG of (Lexing.position)
  | XSTRING_BEG of (Lexing.position)
  | REGEXP_BEG of (Lexing.position)
  | WORDS_BEG of (Lexing.position)
  | QWORDS_BEG of (Lexing.position)
  | STRING_DBEG of (Lexing.position)
  | STRING_DVAR of (Lexing.position)
  | STRING_END of (Lexing.position)
  | EH of (Lexing.position)
  | COLON of (Lexing.position)
  | COMMA of (Lexing.position)
  | SEMI of (Lexing.position)
  | NL
  | SPACE
  | EOF
