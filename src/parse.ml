open Parser

let parse_from_lexbuf lexbuf =
  let state = Lexer_state.state in
    Lexer_state.reset state;
    Parser.program (Lexer.lex state) lexbuf

let parse_from_string s =
  parse_from_lexbuf (Lexing.from_string s)

let parse_from_channel c =
  parse_from_lexbuf (Lexing.from_channel c)
