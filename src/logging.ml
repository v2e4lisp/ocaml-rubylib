let warning msg =
  (* FIXME *)
  (* print_endline ("warning: " ^ msg) *)
  ()

let error msg =
  (* FIXME *)
  (* failwith ("error: " ^ msg) *)
  raise Parsing.Parse_error
