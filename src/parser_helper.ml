module Make (A : Ast.Annot) = struct
  open Ast
  open Lexer_state
  open Logging

  let dummy_annot = A.of_pos dummy_pos
  let empty = Empty dummy_annot

  let yyerror = error

  let rec cond = function
    | Lit (Lit_regexp _, a) as expr ->
        Match (expr, a)
    | And (lhs, rhs, a) ->
        And (cond lhs, cond rhs, a)
    | Or (lhs, rhs, a) ->
        Or (cond lhs, cond rhs, a)
    | Dot2 (lhs, rhs, a) ->
        Flip2 (lhs, rhs, a)
    | Dot3 (lhs, rhs, a) ->
        Flip3 (lhs, rhs, a)
    | expr -> expr

  let logop kind lhs rhs =
    let rec loop lhs rhs =
      match kind, lhs with
      | `And, And (l, r, a) ->
          And (l, loop r rhs, a)
      | `And, _ ->
          And (lhs, rhs, annot_of_expr lhs)
      | `Or, Or (l, r, a) ->
          Or (l, loop r rhs, a)
      | `Or, _ ->
          Or (lhs, rhs, annot_of_expr lhs)
    in loop lhs rhs

  let assoc_list ary =
    (* TODO error check *)
    ary

  let block_append head tail =
    match head, tail with
    | _, Empty _ -> head
    | Empty _, _
        (* Do not drop literals.
           | Lit _, _
           | Str _, _ -> tail
        *)
    | _, _ ->
        match head with
        | Block (body, a) ->
            Block (body @ [tail], a)
        | head ->
            Block ([head; tail], dummy_annot)

  let append_to_block head tail =
    match head, tail with
    | Empty _, _ -> tail
    | _, Empty _ -> head
    | Block (body, a), _ ->
        Block (body @ [tail], a)
    | _, _ ->
        Block ([head; tail], dummy_annot)

  let list_append list item =
    match list with
    | Array (ary, a) ->
        Array (ary @ [item], a)
    | _ ->
        Array ([list; item], dummy_annot)

  let list_prepend item list =
    match list with
    | Array (ary, a) ->
        Array (item :: ary, a)
    | _ ->
        Array ([item; list], dummy_annot)

  let literal_concat head tail =
    let head =
      match head with
      | Evstr (_, a) -> Dstr ([head], a)
      | _ -> head
    in
      match head, tail with
      | Empty _, _ -> tail
      | _, Empty _ -> head
      | Str (s1, a), Str (s2, _) ->
          Str (s1 ^ s2, a)
      | Dstr (list, a), Str _ ->
          Dstr (list @ [tail], a)
      | Str (_, a), Dstr (list, _) ->
          Dstr (head :: list, a)
      | Dstr (l1, a), Dstr (l2, _) ->
          Dstr (l1 @ l2, a)
      | Str (_, a), Evstr _ ->
          Dstr ([head; tail], a)
      | Dstr (l, a), Evstr _ ->
          Dstr (l @ [tail], a)
      | _, _ -> failwith "literal_concat: invalid literals"

  let arg_concat args rest =
    args @ [Splat (rest, annot_of_expr rest)]

  let arg_blk_pass args = function
    | Empty _ -> args
    | blk -> args @ [blk]

  let formal_params norms opts rest block =
    norms @ opts @ rest @ block

  let ret_args args =
    (* error "block argument should not be given" *)
    args

  let new_call ?(block=None) ?(annot=dummy_annot) recv id args =
    let call = Call (recv, id, args, annot) in
      match block with
      | None -> call
      | Some blk ->
          Iter (call, fst blk, snd blk, annot)

  let new_fcall ?(block=None) ?(annot=dummy_annot) id args =
    new_call empty id args ~block ~annot

  let new_vcall ?(annot=dummy_annot) id =
    new_call empty id [] ~annot

  let new_evstr = function
    | Empty a -> Evstr (empty, a)
    | Str _ | Dstr _ | Evstr _ as expr -> expr
    | expr -> Evstr (expr, annot_of_expr expr)

  let new_yield ?(annot=dummy_annot) args =
    (* TODO error "Block argument should not be given." *)
    Yield (args, annot)

  let gettable ?(annot=dummy_annot) = function
    | "self"     -> Self annot
    | "nil"      -> Nil annot
    | "true"     -> True annot
    | "false"    -> False annot
    | "__FILE__" -> Str ("TODO.rb", annot)
    | "__LINE__" -> Lit (Lit_int 42, annot)
    | id         ->
        if Rid.is_class_var id then
          Cvar (id, annot)
        else if Rid.is_instance_var id then
          Ivar (id, annot)
        else if Rid.is_global_var id then
          Gvar (id, annot)
        else if Rid.is_const id then
          Const (id, annot)
        else
          match Env.find state.env id with
          | Some `Lvar ->
              Lvar (id, annot)
          | Some `Dvar ->
              Dvar (id, annot)
          | None ->
              new_vcall id ~annot

  let assignable ?(annot=dummy_annot) id value =
    match id with
    | "nil" | "self" | "true" | "false"
    | "__FILE__" | "__LINE__" ->
        failwith ("Can't change the value of " ^ id)
    | _ ->
        if Env.find state.env id = None then
          Env.add state.env id `Lvar;
        if Rid.is_class_var id then
          if state.in_def > 0 || state.in_single > 0
          then Cvasgn (id, value, annot)
          else Cvdecl (id, value, annot)
        else if Rid.is_instance_var id then
          Iasgn (id, value, annot)
        else if Rid.is_global_var id then
          Gasgn (id, value, annot)
        else if Rid.is_const id then
          Cdecl (id, value, annot)
        else
          match Env.find state.env id with
          | Some `Lvar ->
              Lasgn (id, value, annot)
          | Some `Dvar ->
              if Env.find_in_current state.env id = Some `Dvar then
                Lasgn (id, value, annot)
              else begin
                Env.use state.env id;
                Lasgn (id, value, annot)
              end
          | None ->
              Lasgn (id, value, annot)

  let backref_assign_error = function
    | Nth_ref (nth, _) ->
        error (Printf.sprintf "Can't set variable $%d" nth)
    | Back_ref (c, _) ->
        error (Printf.sprintf "Can't set variable $%c" c)
    | _ ->
        empty

  let node_assign lhs rhs =
    match lhs with
    | Empty _ -> lhs
    | Gasgn (id, _, a) -> Gasgn (id, rhs, a)
    | Iasgn (id, _, a) -> Iasgn (id, rhs, a)
    | Lasgn (id, _, a) -> Lasgn (id, rhs, a)
    | Dasgn (id, _, a) -> Dasgn (id, rhs, a)
    | Masgn (lhs, _, a) -> Masgn (lhs, rhs, a)
    | Cdecl (id, _, a) -> Cdecl (id, rhs, a)
    | Cvdecl (id, _, a) -> Cvdecl (id, rhs, a)
    | Cvasgn (id, _, a) -> Cvasgn (id, rhs, a)
    | Attrasgn (recv, id, args, a) ->
        Attrasgn (recv, id, (args @ [rhs]), a)
    | Call (recv, id, args, a) ->
        Call (recv, id, (args @ [rhs]), a)
    | Const (id, a) -> Cdecl (id, rhs, a)
    | _ -> error "unknown lhs"

  let get_match_node lhs rhs =
    let a = annot_of_expr lhs in
      match lhs, rhs with
      | Dregx _,               _
      | Dregx_once _,          _
      | Lit (Lit_regexp _, _), _
          -> Match2 (lhs, rhs, a)
      | _,                     Dregx _
      | _,                     Dregx_once _
      | _,                     Lit (Lit_regexp _, _)
          -> Match3 (rhs, lhs, a)
      | _,                     _
          -> Call (lhs, "=~", [rhs], a)

  let new_aref ?(annot=dummy_annot) ary args =
    match ary with
    | Self _ ->
        new_fcall "[]" args ~annot
    | _ ->
        new_call ary "[]" args ~annot

  let new_body ?(annot=dummy_annot) body rescues els ensure =
    if rescues <> [] || not (is_empty els) || not (is_empty ensure)
    then Begin ({ body = body;
                  body_rescues = rescues;
                  body_else = els;
                  body_ensure = ensure },
                annot)
    else body

  let new_case ?(annot=dummy_annot) expr whens els =
    Case ({ case_expr = expr;
            case_whens = whens;
            case_else = els },
          annot)

  let new_class ?(annot=dummy_annot) path superclass body =
    Class (path, superclass, body, annot)

  let new_if ?(annot=dummy_annot) test then_ els =
    If (cond test, then_, els, annot)

  let new_masgn ?(wrap=false) ?(annot=dummy_annot) mlhs mrhs =
    match mlhs with
    | [_]
      -> Masgn (Array (mlhs, dummy_annot), Array ([mrhs], dummy_annot), annot)
    | _
      -> Masgn (Array (mlhs, dummy_annot), new_call mrhs "to_ary" [], annot)

  let new_module ?(annot=dummy_annot) path body =
    Module (path, body, annot)

  let new_op_asgn ?(annot=dummy_annot) lhs op arg =
    let set_asgn_value value =
      match lhs with
      | Cvasgn (id, _, a) -> Cvasgn (id, value, a)
      | Cvdecl (id, _, a) -> Cvdecl (id, value, a)
      | Iasgn (id, _, a)  -> Iasgn (id, value, a)
      | Gasgn (id, _, a)  -> Gasgn (id, value, a)
      | Cdecl (id, _, a)  -> Cdecl (id, value, a)
      | Lasgn (id, _, a)  -> Lasgn (id, value, a)
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
      | "||" -> Op_asgn_or (gettable id ~annot, set_asgn_value arg, annot)
      | "&&" -> Op_asgn_and (gettable id ~annot, set_asgn_value arg, annot)
      | _ ->
          let call = new_call (gettable id ~annot) op [arg] ~annot in
            set_asgn_value call

  let new_regexp ?(annot=dummy_annot) expr options =
    (* TODO *)
    empty

  let new_sclass ?(annot=dummy_annot) recv body =
    Sclass (recv, body, annot)

  let new_while ?(annot=dummy_annot) block expr pre =
    let block, pre =
      match block with
      | Begin ({ body = body;
                 body_rescues = [];
                 body_else = _;
                 body_ensure = empty },
               _) -> body, pre
      | _ -> block, false
    in
    let expr = cond expr in
      match expr with
      | Not (e, _) ->
          Until (e, block, pre, annot)
      | _ ->
          While (expr, block, pre, annot)

  let new_until ?(annot=dummy_annot) block expr pre =
    let expr =
      match expr with
      | Not (e, _) -> e
      | _ -> Not (expr, dummy_annot)
    in new_while block expr pre ~annot

  let new_xstring = function
    | Str (str, a) ->
        Xstr (str, a)
    | Dstr (list, a) ->
        Dxstr (list, a)
    | Empty a ->
        Xstr ("", a)
    | str ->
        Dxstr ([str], annot_of_expr str)
end
