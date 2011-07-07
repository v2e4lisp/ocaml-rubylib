let () =
  let expr = Lang_ruby.Parse.parse_from_channel stdin in
    Lang_ruby.Pretty.print_expr expr
