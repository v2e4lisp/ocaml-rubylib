module Make (A : Ast.Annotation) = struct
  module Parser = Ruby18_parser.Make (A)

  (* FIXME: refill lexbuf *)
  let parse_from_lexbuf ?(pos=Lexing.dummy_pos) lexbuf =
    let state = Lexer_state.state in
      Lexer_state.reset state;
      lexbuf.Lexing.lex_start_p <- pos;
      lexbuf.Lexing.lex_curr_p <- pos;
      Parser.program (Lexer.lex state) lexbuf

  let parse_from_string ?(pos=Lexing.dummy_pos) s =
    parse_from_lexbuf (Lexing.from_string s) ~pos

  let parse_from_channel ?(pos=Lexing.dummy_pos) c =
    let m = 1024 in
    let s = String.create m
    and l = ref 0
    and b = Buffer.create m in
      while (l := input c s 0 m; !l != 0) do
        Buffer.add_substring b s 0 !l
      done;
      parse_from_string (Buffer.contents b) ~pos

  let parse_from_file ?(pos=Lexing.dummy_pos) file =
    let c = open_in file in
      try
        let ast = parse_from_channel c ~pos in
          close_in c;
          ast
      with e ->
        close_in c;
        raise e
end

include Make (Ast.Position)
