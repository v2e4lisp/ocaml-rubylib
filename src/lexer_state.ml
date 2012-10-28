open Logging

module Stack_state = struct
  type t = bool list ref

  let create () = ref []

  let clear t = t := []

  let pop t =
    match !t with
      | [] -> false
      | x :: xs -> t := xs; x

  let push t a = t := a :: !t

  let is_in_state t =
    match !t with
      | [] -> false
      | x :: _ -> x

  let lexpop t =
    match !t with
      | a :: b :: xs -> t := (a || b) :: xs
      | _ -> ()

  let of_list = ref

  let to_list t = !t
end

module Env = struct
  module StrSet = Set.Make (String)

  module Scope = struct
    type t = {
      mutable vars : StrSet.t;
      parent : t option;
    }

    let create ?(parent=None) () =
      { vars = StrSet.empty;
        parent = parent }

    let add t id =
      t.vars <- StrSet.add id t.vars

    let rec mem ?(deep=false) t id =
      StrSet.mem id t.vars ||
        match t.parent, deep with
        | Some parent, true ->
            mem ~deep:true parent id
        | _, _ -> false
  end
      
  type t = {
    mutable current : Scope.t;
  }

  let create () =
    { current = Scope.create () }

  let clear t =
    t.current <- Scope.create ()

  let extend ?(dyn=false) t =
    t.current <- Scope.create ~parent:(Some t.current) ()

  let unextend t =
    match t.current.Scope.parent with
    | Some parent -> t.current <- parent
    | None -> ()

  let add t id kind =
    Scope.add t.current id


  let find t id =
    if Scope.mem t.current id
    then Some `Lvar
    else None

  let find_in_current t id =
    if Scope.mem ~deep:false t.current id
    then Some `Lvar
    else None

  let use t id =
    (* TODO *)
    ()
end

type t = {
  mutable cmd_start : bool;
  mutable cmdarg_stack : Stack_state.t;
  cond_stack : Stack_state.t;
  mutable lex_state : lex_state;
  mutable cmd_state : bool;
  mutable last_state : lex_state;
  mutable lex_strterm : lex_strterm option;
  mutable lex_heredoc : lex_heredoc option;
  str_buf : Buffer.t;
  mutable str_nest : int;
  mutable str_end : bool;
  comment : Buffer.t;
  mutable space_seen : bool;
  mutable ruby__end__seen : bool;

  (* parser state *)
  env : Env.t;
  mutable in_def : int;
  mutable in_single : int;
}

and lex_strterm = str_func * char * char

and lex_heredoc = str_func * string * string

and lex_state =
  | Expr_beg
  | Expr_end
  | Expr_endarg
  | Expr_arg
  | Expr_cmdarg
  | Expr_mid
  | Expr_fname
  | Expr_dot
  | Expr_class

and str_func = {
  str_func_escape : bool;
  str_func_expand : bool;
  str_func_regexp : bool;
  str_func_qwords : bool;
  str_func_symbol : bool;
  str_func_indent : bool;
}

let str_squote = {
  str_func_escape = false;
  str_func_expand = false;
  str_func_regexp = false;
  str_func_qwords = false;
  str_func_symbol = false;
  str_func_indent = false
}

and str_dquote = {
  str_func_escape = false;
  str_func_expand = true;
  str_func_regexp = false;
  str_func_qwords = false;
  str_func_symbol = false;
  str_func_indent = false
}

and str_xquote = {
  str_func_escape = false;
  str_func_expand = true;
  str_func_regexp = false;
  str_func_qwords = false;
  str_func_symbol = false;
  str_func_indent = false
}

and str_regexp = {
  str_func_escape = true;
  str_func_expand = true;
  str_func_regexp = true;
  str_func_qwords = false;
  str_func_symbol = false;
  str_func_indent = false
}

and str_sword = {
  str_func_escape = false;
  str_func_expand = false;
  str_func_regexp = false;
  str_func_qwords = true;
  str_func_symbol = false;
  str_func_indent = false
}

and str_dword = {
  str_func_escape = false;
  str_func_expand = true;
  str_func_regexp = false;
  str_func_qwords = true;
  str_func_symbol = false;
  str_func_indent = false
}

and str_ssym = {
  str_func_escape = false;
  str_func_expand = false;
  str_func_regexp = false;
  str_func_qwords = false;
  str_func_symbol = true;
  str_func_indent = false
}

and str_dsym = {
  str_func_escape = false;
  str_func_expand = true;
  str_func_regexp = false;
  str_func_qwords = false;
  str_func_symbol = true;
  str_func_indent = false
}

let merge_str_func f1 f2 =
  { str_func_escape = f1.str_func_escape || f2.str_func_escape;
    str_func_expand = f1.str_func_expand || f2.str_func_expand;
    str_func_regexp = f1.str_func_regexp || f2.str_func_regexp;
    str_func_qwords = f1.str_func_qwords || f2.str_func_qwords;
    str_func_symbol = f1.str_func_symbol || f2.str_func_symbol;
    str_func_indent = f1.str_func_indent || f2.str_func_indent }

let create () = {
  cmd_start         = false;
  cmdarg_stack      = Stack_state.create ();
  cond_stack        = Stack_state.create ();
  lex_state         = Expr_beg;
  cmd_state         = false;
  last_state        = Expr_beg;
  lex_strterm       = None;
  lex_heredoc       = None;
  str_buf           = Buffer.create 32;
  str_nest          = 0;
  str_end           = false;
  comment           = Buffer.create 512;
  space_seen        = false;
  ruby__end__seen   = false;

  env               = Env.create ();
  in_def            = 0;
  in_single         = 0;
}

let reset t =
  t.cmd_start <- false;
  Stack_state.clear t.cmdarg_stack;
  Stack_state.clear t.cond_stack;
  t.lex_state <- Expr_beg;
  t.cmd_state <- false;
  t.last_state <- Expr_beg;
  t.lex_strterm <- None;
  t.lex_heredoc <- None;
  Buffer.reset t.str_buf;
  t.str_nest <- 0;
  t.str_end <- false;
  Buffer.reset t.comment;
  t.space_seen <- false;
  t.ruby__end__seen <- false;
  Env.clear t.env;
  t.in_def <- 0;
  t.in_single <- 0

let is_argument t =
  match t.lex_state with
    | Expr_arg | Expr_cmdarg -> true
    | _ -> false

let arg_ambiguous t =
  warning "Ambiguous first argument. make sure."

let expr_beg_push t =
  Stack_state.push t.cond_stack false;
  Stack_state.push t.cmdarg_stack false;
  t.lex_state <- Expr_beg

let fix_arg_lex_state t =
  match t.lex_state with
    | Expr_fname | Expr_dot
        -> t.lex_state <- Expr_arg
    | _ -> t.lex_state <- Expr_beg

let state = create ()
