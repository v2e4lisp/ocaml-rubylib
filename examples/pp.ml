let () =
  let program = Rubylib.Parser18.parse_from_channel stdin in
    Rubylib.Pretty.print_body program
