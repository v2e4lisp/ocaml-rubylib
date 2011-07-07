include Lexing

let is_bol pos = pos.pos_cnum = pos.pos_bol

let advance lexbuf offset =
  (* TODO re-calculate correct position *)
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + offset

let peek_char lexbuf =
  if (lexbuf.lex_curr_pos >= lexbuf.lex_buffer_len
      && (lexbuf.refill_buff lexbuf;
          lexbuf.lex_eof_reached))
  then None
  else Some lexbuf.lex_buffer.[lexbuf.lex_curr_pos]

let read_char lexbuf =
  match peek_char lexbuf with
  | None -> None
  | Some _ as c -> advance lexbuf 1; c

let peek_char_exn lexbuf =
  match peek_char lexbuf with
  | None -> raise End_of_file
  | Some c -> c

let peek_chars lexbuf n =
  let curr_pos = lexbuf.lex_curr_pos in
  let rec loop = function
    | 0 -> []
    | n ->
        match read_char lexbuf with
        | None -> []
        | Some c -> c :: loop (n-1)
  in
  let chars = loop n in
    lexbuf.lex_curr_pos <- curr_pos;
    chars

let read_char_exn lexbuf =
  match read_char lexbuf with
  | None -> raise End_of_file
  | Some c -> c

let read_char_while lexbuf f =
  let buf = Buffer.create 8 in
  let rec loop () =
    match peek_char lexbuf with
    | Some c when f c ->
        advance lexbuf 1;
        Buffer.add_char buf c;
        loop ()
    | _ -> Buffer.contents buf
  in loop ()

let unread_chars lexbuf s =
  let slen = String.length s in
  let buf = lexbuf.lex_buffer in
  let len = String.length buf in
  let newlen = len + slen in
  let newbuf = String.create newlen in
  let curr_pos = lexbuf.lex_curr_pos in
    String.blit buf 0 newbuf 0 curr_pos;
    String.blit s 0 newbuf curr_pos slen;
    String.blit buf curr_pos newbuf (curr_pos + slen) (len - curr_pos);
    lexbuf.lex_buffer <- newbuf;
    lexbuf.lex_buffer_len <- lexbuf.lex_buffer_len + slen

let test_char lexbuf f =
  match peek_char lexbuf with
  | None -> false
  | Some c -> f c

let skip_char lexbuf c =
  match peek_char lexbuf with
  | Some c' when c = c' ->
      advance lexbuf 1; true
  | _ -> false

let rec skip_char_while lexbuf f =
  let rec loop skipped =
    match peek_char lexbuf with
    | Some c when f c ->
        advance lexbuf 1;
        loop true
    | _ -> skipped
  in loop false

let skip_spaces lexbuf =
  skip_char_while lexbuf
    (function
       | '\n' -> new_line lexbuf; true
       | c -> Rchar.is_space c)
