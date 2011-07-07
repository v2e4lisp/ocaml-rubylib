open Ast
open Lexer_state
open Logging

let dummy_pos = Lexing.dummy_pos

let yyerror = error

let rec cond = function
  | Lit (Lit_regexp _, pos) as expr ->
      Match (expr, pos)
  | And (lhs, rhs, pos) ->
      And (cond lhs, cond rhs, pos)
  | Or (lhs, rhs, pos) ->
      Or (cond lhs, cond rhs, pos)
  | Dot2 (lhs, rhs, pos) ->
      Flip2 (lhs, rhs, pos)
  | Dot3 (lhs, rhs, pos) ->
      Flip3 (lhs, rhs, pos)
  | expr -> expr

let logop kind lhs rhs =
  let rec loop lhs rhs =
    match kind, lhs with
    | `And, And (l, r, pos) ->
        And (l, loop r rhs, pos)
    | `And, _ ->
        And (lhs, rhs, pos_of_expr lhs)
    | `Or, Or (l, r, pos) ->
        Or (l, loop r rhs, pos)
    | `Or, _ ->
        Or (lhs, rhs, pos_of_expr lhs)
  in loop lhs rhs

let assoc_list ary =
  (* TODO error check *)
  ary

let block_append head tail =
  match head, tail with
  | _, Empty -> head
  | Empty, _
  (* Do not drop literals.
  | Lit _, _
  | Str _, _ -> tail
  *)
  | _, _ ->
      match head with
      | Block (body, pos) ->
          Block (body @ [tail], pos)
      | head ->
          Block ([head; tail], dummy_pos)

let append_to_block head tail =
  match head, tail with
  | Empty, _ -> tail
  | _, Empty -> head
  | Block (body, pos), _ ->
      Block (body @ [tail], pos)
  | _, _ ->
      Block ([head; tail], dummy_pos)

let list_append list item =
  match list with
  | Array (ary, pos) ->
      Array (ary @ [item], pos)
  | _ ->
      Array ([list; item], dummy_pos)

let list_prepend item list =
  match list with
  | Array (ary, pos) ->
      Array (item :: ary, pos)
  | _ ->
      Array ([item; list], dummy_pos)

let literal_concat head tail =
  let head =
    match head with
    | Evstr (_, pos) -> Dstr ([head], pos)
    | _ -> head
  in
    match head, tail with
    | Empty, _ -> tail
    | _, Empty -> head
    | Str (s1, pos), Str (s2, _) ->
        Str (s1 ^ s2, pos)
    | Dstr (list, pos), Str _ ->
        Dstr (list @ [tail], pos)
    | Str (_, pos), Dstr (list, _) ->
        Dstr (head :: list, pos)
    | Dstr (l1, pos), Dstr (l2, _) ->
        Dstr (l1 @ l2, pos)
    | Str (_, pos), Evstr _ ->
        Dstr ([head; tail], pos)
    | Dstr (l, pos), Evstr _ ->
        Dstr (l @ [tail], pos)
    | _, _ -> failwith "literal_concat: invalid literals"

let arg_concat args rest =
  args @ [Splat (rest, pos_of_expr rest)]

let arg_blk_pass args = function
  | Empty -> args
  | blk -> args @ [blk]

let formal_params norms opts rest block =
  norms @ opts @ rest @ block

let ret_args args =
  (* error "block argument should not be given" *)
  args

let new_call ?(block=None) ?(pos=dummy_pos) recv id args =
  let call = Call (recv, id, args, pos) in
    match block with
    | None -> call
    | Some blk ->
        Iter (call, fst blk, snd blk, pos)

let new_fcall ?(block=None) ?(pos=dummy_pos) id args =
  new_call Empty id args ~block ~pos

let new_vcall ?(pos=dummy_pos) id =
  new_call Empty id [] ~pos

let new_evstr = function
  | Empty -> Evstr (Empty, dummy_pos)
  | Str _ | Dstr _ | Evstr _ as expr -> expr
  | expr -> Evstr (expr, pos_of_expr expr)

let new_yield ?(pos=dummy_pos) args =
  (* TODO error "Block argument should not be given." *)
  Yield (args, pos)

let gettable ?(pos=dummy_pos) = function
  | "self"     -> Self (pos)
  | "nil"      -> Nil (pos)
  | "true"     -> True (pos)
  | "false"    -> False (pos)
  | "__FILE__" -> Str ("TODO.rb", pos)
  | "__LINE__" -> Lit (Lit_int 42, pos)
  | id         ->
      if Rid.is_class_var id then
        Cvar (id, pos)
      else if Rid.is_instance_var id then
        Ivar (id, pos)
      else if Rid.is_global_var id then
        Gvar (id, pos)
      else if Rid.is_const id then
        Const (id, pos)
      else
        match Env.find state.env id with
        | Some `Lvar ->
            Lvar (id, pos)
        | Some `Dvar ->
            Dvar (id, pos)
        | None ->
            new_vcall id ~pos:pos

let assignable ?(pos=dummy_pos) id value =
  match id with
  | "nil" | "self" | "true" | "false"
  | "__FILE__" | "__LINE__" ->
      failwith ("Can't change the value of " ^ id)
  | _ ->
      if Env.find state.env id = None then
        Env.add state.env id `Lvar;
      if Rid.is_class_var id then
        if state.in_def > 0 || state.in_single > 0
        then Cvasgn (id, value, pos)
        else Cvdecl (id, value, pos)
      else if Rid.is_instance_var id then
        Iasgn (id, value, pos)
      else if Rid.is_global_var id then
        Gasgn (id, value, pos)
      else if Rid.is_const id then
        Cdecl (id, value, pos)
      else
        match Env.find state.env id with
        | Some `Lvar ->
            Lasgn (id, value, pos)
        | Some `Dvar ->
            if Env.find_in_current state.env id = Some `Dvar then
              Lasgn (id, value, pos)
            else begin
              Env.use state.env id;
              Lasgn (id, value, pos)
            end
        | None ->
            Lasgn (id, value, pos)

let backref_assign_error = function
  | Nth_ref (nth, _) ->
      error (Printf.sprintf "Can't set variable $%d" nth)
  | Back_ref (c, _) ->
      error (Printf.sprintf "Can't set variable $%c" c)
  | _ ->
      Empty

let node_assign lhs rhs =
  match lhs with
  | Empty -> Empty
  | Gasgn (id, _, pos) -> Gasgn (id, rhs, pos)
  | Iasgn (id, _, pos) -> Iasgn (id, rhs, pos)
  | Lasgn (id, _, pos) -> Lasgn (id, rhs, pos)
  | Dasgn (id, _, pos) -> Dasgn (id, rhs, pos)
  | Masgn (lhs, _, pos) -> Masgn (lhs, rhs, pos)
  | Cdecl (id, _, pos) -> Cdecl (id, rhs, pos)
  | Cvdecl (id, _, pos) -> Cvdecl (id, rhs, pos)
  | Cvasgn (id, _, pos) -> Cvasgn (id, rhs, pos)
  | Attrasgn (recv, id, args, pos) ->
      Attrasgn (recv, id, (args @ [rhs]), pos)
  | Call (recv, id, args, pos) ->
      Call (recv, id, (args @ [rhs]), pos)
  | Const (id, pos) -> Cdecl (id, rhs, pos)
  | _ -> error "unknown lhs"

let get_match_node lhs rhs =
  let pos = pos_of_expr lhs in
    match lhs, rhs with
    | Dregx _,               _
    | Dregx_once _,          _
    | Lit (Lit_regexp _, _), _
        -> Match2 (lhs, rhs, pos)
    | _,                     Dregx _
    | _,                     Dregx_once _
    | _,                     Lit (Lit_regexp _, _)
        -> Match3 (rhs, lhs, pos)
    | _,                     _
        -> Call (lhs, "=~", [rhs], pos)

let new_aref ?(pos=dummy_pos) ary args =
  match ary with
  | Self _ ->
      new_fcall "[]" args ~pos
  | _ ->
      new_call ary "[]" args ~pos

let new_body ?(pos=dummy_pos) body rescues els ensure =
  if rescues <> [] || els <> Empty || ensure <> Empty
  then Begin ({ body = body;
                body_rescues = rescues;
                body_else = els;
                body_ensure = ensure },
              pos)
  else body

let new_case ?(pos=dummy_pos) expr whens els =
  Case ({ case_expr = expr;
          case_whens = whens;
          case_else = els },
        pos)

let new_class ?(pos=dummy_pos) path superclass body =
  Class (path, superclass, body, pos)

let new_if ?(pos=dummy_pos) test then_ els =
  If (cond test, then_, els, pos)

let new_masgn ?(wrap=false) ?(pos=dummy_pos) mlhs mrhs =
  match mlhs with
  | [_]
    -> Masgn (Array (mlhs, dummy_pos), Array ([mrhs], dummy_pos), pos)
  | _
    -> Masgn (Array (mlhs, dummy_pos), new_call mrhs "to_ary" [], pos)

let new_module ?(pos=dummy_pos) path body =
  Module (path, body, pos)

let new_op_asgn ?(pos=dummy_pos) lhs op arg =
  let set_asgn_value value =
    match lhs with
    | Cvasgn (id, _, pos) -> Cvasgn (id, value, pos)
    | Cvdecl (id, _, pos) -> Cvdecl (id, value, pos)
    | Iasgn (id, _, pos)  -> Iasgn (id, value, pos)
    | Gasgn (id, _, pos)  -> Gasgn (id, value, pos)
    | Cdecl (id, _, pos)  -> Cdecl (id, value, pos)
    | Lasgn (id, _, pos)  -> Lasgn (id, value, pos)
    | _ -> failwith "new_op_asgn: can't replace"
  in
  let id = match lhs with
    | Cvasgn (id, _, _) -> id
    | Cvdecl (id, _, _) -> id
    | Iasgn (id, _, _)  -> id
    | Gasgn (id, _, _)  -> id
    | Cdecl (id, _, _)  -> id
    | Lasgn (id, _, _)  -> id
    | _ -> failwith "new_op_asgn: can't obtain id"
  in
    match op with
    | "||" -> Op_asgn_or (gettable id ~pos, set_asgn_value arg, pos)
    | "&&" -> Op_asgn_and (gettable id ~pos, set_asgn_value arg, pos)
    | _ ->
        let call = new_call (gettable id ~pos) op [arg] ~pos in
          set_asgn_value call

let new_regexp ?(pos=dummy_pos) expr options =
  (* TODO *)
  Empty

let new_sclass ?(pos=dummy_pos) recv body =
  Sclass (recv, body, pos)

let new_while ?(pos=dummy_pos) block expr pre =
  let block, pre =
    match block with
    | Begin ({ body = body;
               body_rescues = [];
               body_else = _;
               body_ensure = Empty },
             _) -> body, pre
    | _ -> block, false
  in
  let expr = cond expr in
    match expr with
    | Not (e, _) ->
        Until (e, block, pre, pos)
    | _ ->
        While (expr, block, pre, pos)

let new_until ?(pos=dummy_pos) block expr pre =
  let expr =
    match expr with
    | Not (e, _) -> e
    | _ -> Not (expr, dummy_pos)
  in new_while block expr pre ~pos

let new_xstring = function
  | Str (str, pos) ->
      Xstr (str, pos)
  | Dstr (list, pos) ->
      Dxstr (list, pos)
  | Empty ->
      Xstr ("", dummy_pos)
  | str ->
      Dxstr ([str], pos_of_expr str)
