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

  let literal_concat head tail =
    match head, tail with
    | Literal (Lit_string head_contents, annot),
      Literal (Lit_string tail_contents, _) ->
        Literal (Lit_string (head_contents @ tail_contents), annot)
    | _, _ ->
        invalid_arg "head, tail"

  let formal_params norms opts rest block =
    norms @ opts @ rest @ block

  let ret_args args =
    (* error "block argument should not be given" *)
    args

  let new_call ?(block=None) ?(annot=dummy_annot) recv id args =
    Call (recv, id, args, block, annot)

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
            Identifier (Id_inst (String.sub id 1 (length - 1)), annot)
          else if Rid.is_global_var id then
            Identifier (Id_glob (String.sub id 1 (length - 1)), annot)
          else if Rid.is_const id then
            Identifier (Id_const (Cpath_name id), annot)
          else
            match Env.find state.env id with
            | Some `Lvar ->
                Identifier (Id_local id, annot)
            | Some `Dvar ->
                Identifier (Id_dyn id, annot)
            | None ->
                new_vcall id ~annot

  let assignable ?(annot=dummy_annot) id =
    match id with
    | "nil" | "self" | "true" | "false"
    | "__FILE__" | "__LINE__" ->
        failwith ("Can't change the value of " ^ id)
    | _ ->
        if Env.find state.env id = None then
          Env.add state.env id `Lvar;
        if Rid.is_class_var id then
          if state.in_def > 0 || state.in_single > 0
          then Lhs_id (Id_class id)
          else Lhs_decl (Id_class id)
        else if Rid.is_instance_var id then
          Lhs_id (Id_inst id)
        else if Rid.is_global_var id then
          Lhs_id (Id_glob id)
        else if Rid.is_const id then
          Lhs_decl (Id_const (Cpath_name id))
        else
          match Env.find state.env id with
          | Some `Lvar ->
              Lhs_id (Id_local id)
          | Some `Dvar ->
              if Env.find_in_current state.env id = Some `Dvar then
                Lhs_id (Id_local id)
              else begin
                Env.use state.env id;
                Lhs_id (Id_local id)
              end
          | None ->
              Lhs_id (Id_local id)

  let get_match_node ?(annot=dummy_annot) lhs rhs =
    match lhs, rhs with
    | Literal (Lit_regexp _, _), _ ->
        Call (lhs, "=~", [Arg_value rhs], None, annot)
    | _, Literal (Lit_regexp _, _) ->
        Call (rhs, "=~", [Arg_value lhs], None, annot)
    | _, _ ->
        Call (lhs, "=~", [Arg_value rhs], None, annot)

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

  let new_op_asgn ?(annot=dummy_annot) lhs op arg =
    let lhs =
      match op with
      | "||" -> Lhs_or lhs
      | "&&" -> Lhs_and lhs
      | _    -> Lhs_op (lhs, op)
    in Assign (lhs, arg, annot)

  let new_regexp ?(annot=dummy_annot) expr options =
    (* TODO *)
    Empty

  let expr_stmt expr = Expr (expr, dummy_annot)
end
