{
  open Logging
  open Token
  open Ruby_lexing
  open Lexer_state

  let start_pos lexbuf = lexbuf.lex_start_p

  let process_id id state lexbuf =
    state.last_state <- state.lex_state;
    let pos = start_pos lexbuf
    and id =
      match read_char lexbuf with
      | Some '!' ->
          if peek_char lexbuf <> Some '='
          then id ^ "!"
          else (advance lexbuf ~-1; id)
      | Some '?' ->
          if peek_char lexbuf <> Some '='
          then id ^ "?"
          else (advance lexbuf ~-1; id)
      | Some _ -> advance lexbuf ~-1; id
      | None -> id in
    let real_id = ref id
    and result = ref EOF
    in
      (match id.[0] with
       | '$' ->
           state.lex_state <- Expr_end;
           result := GVAR (id, pos)
       | '@' ->
           state.lex_state <- Expr_end;
           result :=
             if String.length id >= 2 && id.[1] = '@'
             then CVAR (id, pos)
             else IVAR (id, pos)
       | _ ->
           (match id.[String.length id - 1] with
            | '!' | '?' ->
                result := FID (id, pos)
            | _ ->
                if state.lex_state = Expr_fname then
                  (match peek_chars lexbuf 3 with
                   | ['='; '~'; _] | ['='; '>'; _] -> ()
                   | ['='; '='; c] when c <> '>' -> ()
                   | '=' :: _ ->
                       advance lexbuf 1;
                       real_id := id ^ "=";
                       result := IDENTIFIER (!real_id, pos)
                   | _ -> ());
                if Ruby_id.is_const !real_id then
                  result := CONSTANT (!real_id, pos)
                else
                  result := IDENTIFIER (!real_id, pos));

           match Ruby_keyword.lex_info !real_id with
           | Some (k1, k2, ks) when state.lex_state <> Expr_dot ->
               let k =
                 let st = state.lex_state in
                   state.lex_state <- ks;
                   if k1 = Ruby_keyword.K_do_cond then begin
                     state.cmd_start <- true;
                     if Stack_state.is_in_state state.cond_stack
                     then Ruby_keyword.K_do_cond
                     else if (Stack_state.is_in_state state.cmdarg_stack
                              && st <> Expr_cmdarg)
                     then Ruby_keyword.K_do_block
                     else if st = Expr_endarg
                     then Ruby_keyword.K_do_block
                     else Ruby_keyword.K_do
                   end else if st = Expr_beg then
                     k1
                   else begin
                     if k1 <> k2 then
                       state.lex_state <- Expr_beg;
                     k2
                   end
               in
                 result := Ruby_keyword.mk_token k ~pos
           | _ ->
               (match state.lex_state with
                | Expr_beg | Expr_mid | Expr_dot
                | Expr_arg | Expr_cmdarg ->
                    if state.cmd_state then
                      state.lex_state <- Expr_cmdarg
                    else
                      state.lex_state <- Expr_arg
                | _ -> state.lex_state <- Expr_end));

      if (state.last_state <> Expr_dot
          && Env.find state.env !real_id = Some `Lvar) then
        state.lex_state <- Expr_end;

      !result
}

(* epsilon *)
let e = ""
let oct = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let ident0 = ['a'-'z' 'A'-'Z'] | '_' | [^ '\000'-'\177']
let ident = (ident0 | ['0'-'9'])
let space = [' ' '\t' '\012' '\r' '\011']
let newline = '\n'

rule token state = parse
  | e { token_case state lexbuf }

and token_case state = parse
  | '\000'
  | '\004'
  | '\026'
  | eof { EOF }

  | space+
      { state.space_seen <- true;
        token state lexbuf }

  | '#' ([^ '\n']* as comment) '\n'
      { new_line lexbuf;
        read_comment state comment lexbuf }
  | newline
      { new_line lexbuf;
        read_newline state lexbuf }

  | "**=" { state.lex_state <- Expr_beg;
            OP_ASGN ("**", start_pos lexbuf) }
  | "**" { fix_arg_lex_state state;
           POW (start_pos lexbuf) }
  | "*=" { state.lex_state <- Expr_beg;
           OP_ASGN ("*", start_pos lexbuf) }
  | '*' { let tok =
            if (is_argument state
                && state.space_seen
                && not (test_char lexbuf Ruby_char.is_space))
            then begin
              warning "`*' interpreted as argument prefix";
              STAR (start_pos lexbuf)
            end else if (state.lex_state = Expr_beg
                || state.lex_state = Expr_mid)
            then STAR (start_pos lexbuf)
            else STAR2 (start_pos lexbuf)
          in fix_arg_lex_state state; tok }

  | "!=" { state.lex_state <- Expr_beg;
           NEQ (start_pos lexbuf) }
  | "!~" { state.lex_state <- Expr_beg;
           NMATCH (start_pos lexbuf) }
  | '!' { state.lex_state <- Expr_beg;
          BANG (start_pos lexbuf) }

  | "===" { fix_arg_lex_state state;
            EQQ (start_pos lexbuf) }
  | "==" { fix_arg_lex_state state;
           EQ (start_pos lexbuf) }
  | "=~" { fix_arg_lex_state state;
           MATCH (start_pos lexbuf) }
  | "=>" { fix_arg_lex_state state;
           ASSOC (start_pos lexbuf) }
  | "=begin" (space | '\n') _* "\n=end" space* ('\n' | eof)
      { let count_lines s =
          let n = ref 0 in
            String.iter
              (fun c -> if c = '\n' then n := !n + 1)
              s;
            !n
        in
        let lines = count_lines (lexeme lexbuf) in
        let pos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <-
            { pos with
                pos_bol = pos.pos_cnum;
                pos_lnum = pos.pos_lnum + lines };
          if is_bol lexbuf.lex_start_p then
            token state lexbuf
          else
            failwith "you shouldn't be able to get here" }
  | "=" { fix_arg_lex_state state;
          EQL (start_pos lexbuf) }

  | "<=>" { fix_arg_lex_state state;
            CMP (start_pos lexbuf) }
  | "<=" {  fix_arg_lex_state state;
            LEQ (start_pos lexbuf) }
  | "<<=" { fix_arg_lex_state state;
            state.lex_state <- Expr_beg;
            OP_ASGN ("<<", start_pos lexbuf) }
  | "<<" {
      if ((match state.lex_state with
           | Expr_end | Expr_dot | Expr_endarg | Expr_class -> false
           | _ -> true)
          && (not (is_argument state) || state.space_seen)) then
        match heredoc_identifier state lexbuf with
        | Some tok -> tok
        | None ->
            fix_arg_lex_state state;
            LSHFT (start_pos lexbuf)
      else begin
        fix_arg_lex_state state;
        LSHFT (start_pos lexbuf)
      end }
  | '<' { fix_arg_lex_state state;
          LT (start_pos lexbuf) }

  | ">=" { fix_arg_lex_state state;
           GEQ (start_pos lexbuf) }
  | ">>=" { fix_arg_lex_state state;
            state.lex_state <- Expr_beg;
            OP_ASGN (">>", start_pos lexbuf) }
  | ">>" { fix_arg_lex_state state;
           RSHFT (start_pos lexbuf) }
  | '>' { fix_arg_lex_state state;
          GT (start_pos lexbuf) }

  | '"' { state.lex_strterm <- Some (str_dquote, '"', '\000');
          STRING_BEG (start_pos lexbuf) }

  | '`' { match state.lex_state with
          | Expr_fname ->
              state.lex_state <- Expr_end;
              BACK_REF2 (start_pos lexbuf)
          | Expr_dot ->
              if state.cmd_state then
                state.lex_state <- Expr_cmdarg
              else
                state.lex_state <- Expr_arg;
              BACK_REF2 (start_pos lexbuf)
          | _ ->
              state.lex_strterm <- Some (str_xquote, '`', '\000');
              XSTRING_BEG (start_pos lexbuf) }

  | '\'' { state.lex_strterm <- Some (str_squote, '\'', '\000');
           STRING_BEG (start_pos lexbuf) }

  | '?' { match state.lex_state with
          | Expr_end | Expr_endarg ->
              state.lex_state <- Expr_beg;
              EH (start_pos lexbuf)
          | _ -> parse_eh state lexbuf }

  | "&&=" { state.lex_state <- Expr_beg;
            OP_ASGN ("&&", start_pos lexbuf) }
  | "&&" { state.lex_state <- Expr_beg;
           ANDOP (start_pos lexbuf) }
  | "&=" { state.lex_state <- Expr_beg;
           OP_ASGN ("&", start_pos lexbuf) }
  | '&' { let tok =
            if (is_argument state
                && state.space_seen
                && not (test_char lexbuf Ruby_char.is_space))
            then begin
              warning "`&' interpreted as argument prefix";
              AMPER (start_pos lexbuf)
            end else if (state.lex_state = Expr_beg
                || state.lex_state = Expr_mid)
            then AMPER (start_pos lexbuf)
            else AMPER2 (start_pos lexbuf)
          in fix_arg_lex_state state; tok }

  | "||=" { state.lex_state <- Expr_beg;
            OP_ASGN ("||", start_pos lexbuf) }
  | "||" { state.lex_state <- Expr_beg;
           OROP (start_pos lexbuf) }
  | "|=" { state.lex_state <- Expr_beg;
           OP_ASGN ("|", start_pos lexbuf) }
  | '|' { fix_arg_lex_state state;
          PIPE (start_pos lexbuf) }

  | ['+' '-'] as sign
      { let ucons =
          if sign = '+'
          then (fun pos -> UPLUS (pos))
          else (fun pos -> UMINUS (pos))
        and cons =
          if sign = '+'
          then (fun pos -> PLUS (pos))
          else (fun pos -> MINUS (pos))
        in
          if state.lex_state = Expr_fname || state.lex_state == Expr_dot then begin
            state.lex_state <- Expr_arg;
            if skip_char lexbuf '@' then
              ucons (start_pos lexbuf)
            else
              cons (start_pos lexbuf)
          end else begin
            if skip_char lexbuf '=' then begin
              state.lex_state <- Expr_beg;
              OP_ASGN (Char.escaped sign, start_pos lexbuf)
            end else if (state.lex_state = Expr_beg
                || state.lex_state = Expr_mid
                || (is_argument state
                    && state.space_seen
                    && not (test_char lexbuf Ruby_char.is_space)))
            then begin
              if is_argument state then
                arg_ambiguous state;
              state.lex_state <- Expr_beg;

              if test_char lexbuf Ruby_char.is_digit then
                if sign = '+' then
                  parse_number state lexbuf
                else
                  UMINUS_NUM (start_pos lexbuf)
              else
                ucons (start_pos lexbuf)
            end else begin
              state.lex_state <- Expr_beg;
              cons (start_pos lexbuf)
            end
          end }

  | "..." { state.lex_state <- Expr_beg;
            DOT3 (start_pos lexbuf) }
  | ".." { state.lex_state <- Expr_beg;
           DOT2 (start_pos lexbuf) }
  | '.' { if test_char lexbuf Ruby_char.is_digit then
            error "no .<digit> floating literal anymore put 0 before dot";
          state.lex_state <- Expr_dot;
          DOT (start_pos lexbuf) }

  | ['0'-'9']
      { advance lexbuf ~-1;
        parse_number state lexbuf }

  | (')' | ']' | '}') as c
      { Stack_state.lexpop state.cond_stack;
        Stack_state.lexpop state.cmdarg_stack;
        state.lex_state <- Expr_end;
        match c with
        | ')' -> RPAREN (start_pos lexbuf)
        | ']' -> RBRACK (start_pos lexbuf)
        | '}' -> RCURLY (start_pos lexbuf)
        | _ -> failwith "never reach here" }

  | "::" { if (state.lex_state = Expr_beg
               || state.lex_state == Expr_mid
               || state.lex_state == Expr_class
               || (is_argument state && state.space_seen))
           then begin
             state.lex_state <- Expr_beg;
             COLON3 (start_pos lexbuf)
           end else begin
             state.lex_state <- Expr_dot;
             COLON2 (start_pos lexbuf)
           end }
  | ':' { if (state.lex_state = Expr_end
              || state.lex_state = Expr_endarg
              || test_char lexbuf Ruby_char.is_space)
          then begin
            state.lex_state <- Expr_beg;
            COLON (start_pos lexbuf)
          end else begin
            (match read_char lexbuf with
             | Some '\'' ->
                 state.lex_strterm <- Some (str_ssym, '\'', '\000')
             | Some '"' ->
                 state.lex_strterm <- Some (str_dsym, '"', '\000')
             | Some _ ->
                 advance lexbuf ~-1;
                 state.lex_state <- Expr_fname
             | None ->
                 state.lex_state <- Expr_fname);
            SYMBEG (start_pos lexbuf)
          end }

  | '/' { if state.lex_state = Expr_beg || state.lex_state = Expr_mid then begin
            state.lex_strterm <- Some (str_regexp, '/', '\000');
            REGEXP_BEG (start_pos lexbuf)
          end else if peek_char lexbuf = Some '=' then begin
            state.lex_state <- Expr_beg;
            OP_ASGN ("/", start_pos lexbuf)
          end else if (is_argument state
                       && state.space_seen
                       && not (test_char lexbuf Ruby_char.is_space))
          then begin
            arg_ambiguous state;
            state.lex_strterm <- Some (str_regexp, '/', '\000');
            REGEXP_BEG (start_pos lexbuf)
          end else begin
            fix_arg_lex_state state;
            DIVIDE (start_pos lexbuf)
          end }

  | "^=" { state.lex_state <- Expr_beg;
           OP_ASGN ("^", start_pos lexbuf) }
  | '^' { fix_arg_lex_state state;
          CARET (start_pos lexbuf) }

  | ';' { state.cmd_start <- true;
          state.lex_state <- Expr_beg;
          SEMI (start_pos lexbuf) }

  | ',' { state.lex_state <- Expr_beg;
          COMMA (start_pos lexbuf) }

  | '~' { if (state.lex_state = Expr_fname
              || state.lex_state = Expr_dot) then
            ignore (skip_char lexbuf '@');
          fix_arg_lex_state state;
          TILDE (start_pos lexbuf) }

  | '(' { state.cmd_start <- true;
          let tok =
            if (state.lex_state = Expr_beg
                || state.lex_state = Expr_mid) then
              LPAREN (start_pos lexbuf)
            else if state.space_seen then
              if state.lex_state = Expr_cmdarg then
                LPAREN_ARG (start_pos lexbuf)
              else begin
                warning "don't put space before argument parentheses";
                LPAREN2 (start_pos lexbuf)
              end
            else
              LPAREN2 (start_pos lexbuf)
          in expr_beg_push state; tok }

  | '[' { if state.lex_state = Expr_fname || state.lex_state = Expr_dot then begin
            state.lex_state <- Expr_arg;
            parse_lbrack state lexbuf
          end else
            let tok = 
              if (state.lex_state = Expr_beg
                  || state.lex_state = Expr_mid
                  || (is_argument state
                      && state.space_seen))
              then LBRACK (start_pos lexbuf)
              else LB (start_pos lexbuf)
            in
              expr_beg_push state; tok }

  | '{' { let tok =
            if is_argument state || state.lex_state = Expr_end
            then LCURLY (start_pos lexbuf)
            else if state.lex_state = Expr_endarg
            then LBRACE_ARG (start_pos lexbuf)
            else LBRACE (start_pos lexbuf)
          in expr_beg_push state; tok }

  | "\\\n" { state.space_seen <- true;
             token state lexbuf }
  | '\\' { error "bare backslash only allowed before newline" }

  | '%' { if state.lex_state = Expr_beg || state.lex_state = Expr_mid
          then parse_quote state lexbuf
          else if skip_char lexbuf '=' then begin
            state.lex_state <- Expr_beg;
            OP_ASGN ("%", start_pos lexbuf)
          end else if (is_argument state
                       && state.space_seen
                       && not (test_char lexbuf Ruby_char.is_space))
          then parse_quote state lexbuf
          else begin
            fix_arg_lex_state state;
            PERCENT (start_pos lexbuf)
          end }
      
  | "$_" ident+ as id
      { process_id id state lexbuf }
  | "$_" as id
      { GVAR (id, start_pos lexbuf) }
  | '$' ('-' ident? |
             ['~' '*' '$' '?' '!' '@'
                '/' '\\' ';' ',' '.'
                '=' ':' '<' '>' '"']) as id
      { state.lex_state <- Expr_end;
        GVAR (id, start_pos lexbuf) }
  | '$' (['&' '`' '\'' '+'] | ['1'-'9'] ['0'-'9']*) as id
      { state.lex_state <- Expr_end;
        GVAR (id, start_pos lexbuf) }
  | "$0" as id
      { process_id id state lexbuf }
  | '$' ident+ as id
      { process_id id state lexbuf }

  | '@' '@'? ident* as id
      { process_id id state lexbuf }

  | ("__END__" | '_' ident*) as id
      { match id with
        | "__END__" when is_bol lexbuf.lex_start_p ->
            state.ruby__end__seen <- true;
            EOF
        | _ ->
            process_id id state lexbuf }

  | ident0 ident* as id
      { process_id id state lexbuf }

and parse_eh state = parse
  | eof { error "incomplete character syntax" }
  | (space | '\n') as c
      { if not (is_argument state) then
          (match
             match c with
             | ' '    -> Some 's'
             | '\n'   -> new_line lexbuf; Some 'n'
             | '\t'   -> Some 't'
             | '\011' -> Some 'f'
             | '\r'   -> Some 'r'
             | '\012' -> Some 'v'
             | _      -> None
           with
           | Some c ->
               warning (Printf.sprintf "invalid character syntax; use ?\\%c" c)
           | None -> ());
        state.lex_state <- Expr_beg;
        EH (start_pos lexbuf) }
  | ident ident
      { advance lexbuf ~-2;
        state.lex_state <- Expr_beg;
        EH (start_pos lexbuf) }
  | '\\'
      { let c = read_escape state lexbuf in
        let n = Char.code c in
          state.lex_state <- Expr_end;
          INTEGER (n land 0xff, start_pos lexbuf) }
  | _ as c
      { state.lex_state <- Expr_end;
        INTEGER ((Char.code c) land 0xff, start_pos lexbuf) }

and parse_lbrack state = parse
  | "]=" { ASET (start_pos lexbuf) }
  | ']' { AREF (start_pos lexbuf) }

and parse_number state = parse
  | '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F' '_']+
  | '0' ['b' 'B'] ['0' '1' '_']+
  | '0' ['d' 'D'] ['0'-'9' '_']+
  | '0' ['o' 'O' '_'] ['0'-'7' '_']+
  | ['0'-'9'] ['0'-'9' '_']*
      { state.lex_state <- Expr_end;
        (* FIXME: bigint *)
        let int =
          try int_of_string (lexeme lexbuf)
          with _ -> (* FIXME: parser *) 0
        in
          INTEGER (int, start_pos lexbuf) }
  | ['0'-'9'] ['0'-'9' '_']* '.' ['0'-'9']+ (['e' 'E'] ['-' '+']? ['0'-'9' '_']+)?
      { state.lex_state <- Expr_end;
        FLOAT (float_of_string (lexeme lexbuf), start_pos lexbuf) }

and parse_quote state = parse
  | (['Q' 'q' 'W' 'w' 'x' 'r' 's']? as c)
      ([^ 'a'-'z' 'A'-'Z' '0'-'9'] as term)
      { let c = if c = "" then 'Q' else c.[0]
        and paren, term =
          match term with
          | '(' -> term, ')'
          | '[' -> term, ']'
          | '{' -> term, '}'
          | '<' -> term, '>'
          | _   -> '\000', term
        in match c with
        | 'Q' ->
          state.lex_strterm <- Some (str_dquote, term, paren);
          STRING_BEG (start_pos lexbuf)
        | 'q' ->
          state.lex_strterm <- Some (str_squote, term, paren);
          STRING_BEG (start_pos lexbuf)
        | 'W' ->
          state.lex_strterm <- Some (str_dword, term, paren);
          WORDS_BEG (start_pos lexbuf)
        | 'w' ->
          ignore (skip_spaces lexbuf);
          state.lex_strterm <- Some (str_sword, term, paren);
          QWORDS_BEG (start_pos lexbuf)
        | 'x' ->
          state.lex_strterm <- Some (str_xquote, term, paren);
          XSTRING_BEG (start_pos lexbuf)
        | 'r' ->
          state.lex_strterm <- Some (str_regexp, term, paren);
          REGEXP_BEG (start_pos lexbuf)
        | 's' ->
          state.lex_strterm <- Some (str_ssym, term, paren);
          SYMBEG (start_pos lexbuf)
        | _ ->
          error "unknown type of %string" }

and read_comment state comment = parse
  | e { match state.lex_state with
        | Expr_beg -> COMMENT (comment, start_pos lexbuf)
        | _        -> read_newline state lexbuf }
  
and read_newline state = parse
  | e { match state.lex_state with
        | Expr_beg | Expr_fname
        | Expr_dot | Expr_class
            -> token state lexbuf
        | _ -> read_newline' state lexbuf }

and read_newline' state = parse
  | space+
      { state.space_seen <- true;
        read_newline' state lexbuf }
  | '.' [^ '.']
      { advance lexbuf ~-2;
        token state lexbuf }
  | e
  | eof
      { state.cmd_start <- true;
        state.lex_state <- Expr_beg;
        NL }

and read_escape state = parse
  | '\\' { '\\' }
  | 'n' { '\n' }
  | 't' { '\t' }
  | 'r' { '\r' }
  | 'f' { '\012' }
  | 'v' { '\011' }
  | 'a' { '\007' }
  | 'e' { '\027' }
  | 'b' { '\010' }
  | 's' { ' ' }
  | oct oct? oct? as oct
      { Char.chr (int_of_string ("0o" ^ oct)) }
  | 'x' hex hex? as hex
      { Char.chr (int_of_string ("0" ^ hex)) }
  | "M-"
      { escape_char 0x80 state lexbuf }
  | ("C-?" | "c?")
      { Char.chr 127 }
  | ("C-" | 'c')
      { escape_char 0x9f state lexbuf }
  | _ as c { c }

and escape_char mask state = parse
  | _ as c
      { Char.chr
          (mask lor
             (Char.code
                (if c = '\\'
                 then read_escape state lexbuf
                 else c))) }

and tokadd_escape state = parse
  | "\\\n"
      { () }
  | '\\' oct oct? oct?
  | "\\x" hex hex?
      { Buffer.add_string state.str_buf (lexeme lexbuf) }
  | '\\' ((['M' 'C'] '-' | 'c') as pre) '\\'
      { advance lexbuf ~-1;
        Buffer.add_string state.str_buf pre;
        tokadd_escape state lexbuf }
  | '\\' (['M' 'C'] '-' | 'c') _
      { Buffer.add_string state.str_buf (lexeme lexbuf) }
  | '\\' ['M' 'c' 'C' 'x']
      { error "Invalid escape character syntax" }
  | _ as c
      { Buffer.add_char state.str_buf c }

and heredoc here state = parse
  | eof
      { error "no end of heredoc" }
  | ([' ' '\t']* as indent) (ident+ as id) ('\r'? '\n' | eof)
      { new_line lexbuf;
        let func, hid,line = here in
          if (is_bol lexbuf.lex_start_p
              && hid = id
              && (func.str_func_indent
                  || indent = ""))
          then begin
            (* TODO fix pos *)
            unread_chars lexbuf line;
            STRING_END (start_pos lexbuf)
          end else
            STRING_CONTENT (lexeme lexbuf, start_pos lexbuf) }
  | [^ '\n']* ('\n' | eof)
      { new_line lexbuf;
        STRING_CONTENT (lexeme lexbuf, start_pos lexbuf) }

and heredoc_identifier state = parse
  | ('-'? as indent) ('\'' as term) ([^ '\'']* as id) '\''
  | ('-'? as indent) ('"' as term) ([^ '"']* as id) '"'
  | ('-'? as indent) ('`' as term) ([^ '`']* as id) '`'
      { let func = str_squote in
        let func = 
          if indent = "" then
            func
          else
            { func with str_func_indent = true }
        in
        let func =
          merge_str_func func
            (match term with
             | '\'' -> str_squote
             | '\"' -> str_dquote
             | '`'  -> str_xquote
             | _    -> failwith "never reach here")
        in Some (heredoc_identifier' term id func state lexbuf) }
  | ('-'? as indent) (ident+ as id)
       { let func = str_dquote in
         let func =
           if indent = "" then
             func
           else
             { func with str_func_indent = true }
         in Some (heredoc_identifier' ('"') id func state lexbuf) }
  | e { None }

and heredoc_identifier' term id func state = parse
  | [^ '\n']* '\n' as line
      { state.lex_heredoc <- Some (func, id, line);
        match term with
        | '`' -> XSTRING_BEG (start_pos lexbuf)
        | _   -> STRING_BEG (start_pos lexbuf) }

{
  let regx_options lexbuf =
    read_char_while lexbuf
      (function
         | 'i' | 'x' | 'm' | 'o' | 'n' | 'e' | 's' | 'u' -> true
         | _ -> false)

  let tokadd_string strterm state lexbuf =
    let func, term, paren = strterm in
    let rec next c =
      Buffer.add_char state.str_buf c;
      work ()
    and work () =
      let c = read_char_exn lexbuf in
        if paren <> '\000' && c = paren then begin
          state.str_nest <- succ state.str_nest;
          next c
        end else if c = term then
          if state.str_nest = 0 then
            advance lexbuf ~-1
          else begin
            state.str_nest <- pred state.str_nest;
            next c
          end
        else if func.str_func_expand && c = '#' then
          match peek_char lexbuf with
          | Some '$' | Some '@' | Some '{' ->
              advance lexbuf ~-1
          | _ -> next c
        else if c = '\\' then
          (match read_char_exn lexbuf with
           | '\n' ->
               new_line lexbuf;
               if func.str_func_qwords then
                 next c
               else if func.str_func_expand then
                 work ()
               else begin
                 Buffer.add_char state.str_buf '\\';
                 next '\n'
               end
           | '\\' ->
               if func.str_func_escape then begin
                 Buffer.add_char state.str_buf '\\';
                 next '\\'
               end else
                 next '\\'
           | c ->
               if func.str_func_regexp then begin
                 advance lexbuf ~-1;
		 tokadd_escape state lexbuf;
                 work ()
               end else if func.str_func_expand then begin
                 advance lexbuf ~-1;
                 if func.str_func_escape then
                   Buffer.add_char state.str_buf '\\';
                 next (read_escape state lexbuf)
               end else if func.str_func_qwords && Ruby_char.is_space c then begin
                 if c = '\n' then
                   new_line lexbuf;
                 next c
               end else if c <> term && (paren = '\000' || c <> paren) then begin
                 Buffer.add_char state.str_buf '\\';
                 next '\\'
               end)
        else if func.str_func_qwords && Ruby_char.is_space c then
          advance lexbuf ~-1
        else
          next c
      in work ()
           
  let parse_string strterm state lexbuf =
    let func, term, paren = strterm in
    let space =
      (func.str_func_qwords && skip_spaces lexbuf)
    in
      if (state.str_nest = 0 && skip_char lexbuf term) then
        if func.str_func_qwords then begin
          state.str_end <- true;
          SPACE
        end else if func.str_func_regexp then
          REGEXP_END (regx_options lexbuf, start_pos lexbuf)
        else
          STRING_END (start_pos lexbuf)
      else if space then
        SPACE
      else begin
        Buffer.reset state.str_buf;
        match
          if func.str_func_expand && skip_char lexbuf '#' then
            (match peek_char lexbuf with
             | Some '$' | Some '@' ->
                 Some (STRING_DVAR (start_pos lexbuf))
             | Some '{' ->
                 advance lexbuf 1;
                 Some (STRING_DBEG (start_pos lexbuf))
             | _ ->
                 Buffer.add_char state.str_buf '#';
                 None)
          else None
        with
        | Some tok -> tok
        | None ->
            (try tokadd_string strterm state lexbuf
             with End_of_file ->
               error "unterminated string meets end of file");
            STRING_CONTENT (Buffer.contents state.str_buf, start_pos lexbuf)
      end

  let lex_string state lexbuf =
    let tok =
      if state.str_end then
        STRING_END (start_pos lexbuf)
      else
        match state.lex_strterm, state.lex_heredoc with
        | _, Some here ->
            heredoc here state lexbuf
        | Some strterm, _ ->
            parse_string strterm state lexbuf
        | _, _ ->
            failwith "never reach here"
    in
      (match tok with
       | STRING_END (_) | REGEXP_END (_) ->
           state.lex_strterm <- None;
           state.lex_heredoc <- None;
           state.lex_state <- Expr_end;
           state.str_end <- false
       | _ -> ());
      tok

  let lex state lexbuf =
    state.space_seen <- false;
    if (state.lex_strterm <> None
        || state.lex_heredoc <> None) then
      lex_string state lexbuf
    else begin
      state.cmd_state <- state.cmd_start;
      state.cmd_start <- false;
      state.last_state <- state.lex_state;
      token state lexbuf
    end
}
