module Make (A : Ast.Annot) = struct
  open Ast
  open Lexer_state
  open Logging

  let dummy_annot = A.of_pos dummy_pos
  let annot_of_expr expr =
    if expr = Empty
    then dummy_annot
    else annot_of_expr expr

  let yyerror = error

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
    match head, tail with
    | Literal (Lit_string head_contents, annot),
      Literal (Lit_string tail_contents, _) ->
        Literal (Lit_string (head_contents @ tail_contents), annot)
    | _, _ ->
        invalid_arg "head, tail"

  let arg_concat args rest =
    args @ [Splat (rest, annot_of_expr rest)]

  let arg_blk_pass args = function
    | Empty -> args
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
    new_call Empty id args ~block ~annot

  let new_vcall ?(annot=dummy_annot) id =
    new_call Empty id [] ~annot

  let new_yield ?(annot=dummy_annot) args =
    (* TODO error "Block argument should not be given." *)
    Yield (args, annot)

  let gettable ?(annot=dummy_annot) = function
    | "nil"      -> Identifier (Id_pseudo Pid_nil, annot)
    | "true"     -> Identifier (Id_pseudo Pid_true, annot)
    | "false"    -> Identifier (Id_pseudo Pid_false, annot)
    | "self"     -> Identifier (Id_pseudo Pid_self, annot)
    | "__FILE__" -> Literal (Lit_string [Str_contents "__FILE__"], annot)
    | "__LINE__" -> Literal (Lit_integer 42, annot)
    | id         ->
        let length = String.length id in
          if Rid.is_class_var id then
            Identifier (Id_class (String.sub id 2 (length - 2)), annot)
          else if Rid.is_instance_var id then
            Identifier (Id_instance (String.sub id 1 (length - 1)), annot)
          else if Rid.is_global_var id then
            Identifier (Id_global (String.sub id 1 (length - 1)), annot)
          else if Rid.is_const id then
            Identifier (Id_constant id, annot)
          else
            match Env.find state.env id with
            | Some `Lvar ->
                Identifier (Id_local id, annot)
            | Some `Dvar ->
                Identifier (Id_dynamic id, annot)
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
          then Assign (Id_class id, value, annot)
          else Declare (Id_class id, value, annot)
        else if Rid.is_instance_var id then
          Assign (Id_instance id, value, annot)
        else if Rid.is_global_var id then
          Assign (Id_global id, value, annot)
        else if Rid.is_const id then
          Declare (Id_constant id, value, annot)
        else
          match Env.find state.env id with
          | Some `Lvar ->
              Assign (Id_local id, value, annot)
          | Some `Dvar ->
              if Env.find_in_current state.env id = Some `Dvar then
                Assign (Id_local id, value, annot)
              else begin
                Env.use state.env id;
                Assign (Id_local id, value, annot)
              end
          | None ->
              Assign (Id_local id, value, annot)

  let node_assign lhs rhs =
    match lhs with
    | Empty -> lhs
    | Declare (id, _, a) -> Declare (id, rhs, a)
    | Assign (id, _, a) -> Assign (id, rhs, a)
    | Massign (id, _, a) -> Massign (id, rhs, a)
    | Attrasgn (recv, id, args, a) ->
        Attrasgn (recv, id, (args @ [rhs]), a)
    | Call (recv, id, args, a) ->
        Call (recv, id, (args @ [rhs]), a)
    | Const (id, a) -> Declare (Id_constant id, rhs, a)
    | _ -> error "unknown lhs"

  let get_match_node ?(annot=dummy_annot) lhs rhs =
    match lhs, rhs with
    | Literal (Lit_regexp _, _), _ ->
        Call (lhs, "=~", [rhs], annot)
    | _, Literal (Lit_regexp _, _) ->
        Call (rhs, "=~", [lhs], annot)
    | _, _ ->
        Call (lhs, "=~", [rhs], annot)

  let new_aref ?(annot=dummy_annot) ary args =
    match ary with
    | Identifier (Id_pseudo Pid_self, _) ->
        new_fcall "[]" args ~annot
    | _ ->
        new_call ary "[]" args ~annot

  let new_case ?(annot=dummy_annot) expr whens els =
    Case ({ case_expr = expr;
            case_whens = whens;
            case_else = els },
          annot)

  let new_class ?(annot=dummy_annot) path superclass body =
    Class (path, superclass, body, annot)

  let new_masgn ?(wrap=false) ?(annot=dummy_annot) mlhs mrhs =
    match mlhs with
    | [_]
      -> Massign (Array (mlhs, dummy_annot), Array ([mrhs], dummy_annot), annot)
    | _
      -> Massign (Array (mlhs, dummy_annot), new_call mrhs "to_ary" [], annot)

  let new_module ?(annot=dummy_annot) path body =
    Module (path, body, annot)

  let new_op_asgn ?(annot=dummy_annot) lhs op arg =
    let set_asgn_value value =
      match lhs with
      | Declare (id, _, a) -> Declare (id, value, a)
      | Assign (id, _, a) -> Assign (id, value, a)
      | Massign (id, _, a) -> Massign (id, value, a)
      | _ -> failwith "new_op_asgn: can't replace"
    in
    let id = match lhs with
      | Declare (id, _, _) -> id
      | Assign (id, _, _) -> id
      | _ -> failwith "new_op_asgn: can't obtain id"
    in
    let id = string_of_identifier id in
      match op with
      | "||" -> Op_asgn_or (gettable id ~annot, set_asgn_value arg, annot)
      | "&&" -> Op_asgn_and (gettable id ~annot, set_asgn_value arg, annot)
      | _ ->
          let call = new_call (gettable id ~annot) op [arg] ~annot in
            set_asgn_value call

  let new_regexp ?(annot=dummy_annot) expr options =
    (* TODO *)
    Empty

  let expr_stmt expr = Expr (expr, dummy_annot)
end
