let is_digit =  function
  | '0' | '1' | '2' | '3' | '4'
  | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

let is_space = function
  | ' ' | '\t' | '\012' | '\r' | '\011' | '\n' -> true
  | _ -> false

let is_const c = 'A' <= c && c <= 'Z'
