let is_const id =
  String.length id >= 1
  && Rchar.is_const id.[0]

let is_class_var id =
  String.length id >= 2
  && id.[0] == '@'
  && id.[1] == '@'

let is_instance_var id =
  not (is_const id)
  && String.length id >= 1
  && id.[0] == '@'

let is_global_var id =
  String.length id >= 1
  && id.[0] == '$'
