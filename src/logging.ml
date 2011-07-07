let warning msg =
  print_endline ("warning: " ^ msg)

let error msg =
  failwith ("error: " ^ msg)
