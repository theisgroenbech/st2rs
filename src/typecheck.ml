open Types

type resultOrError =
    Result of tenv
  | Error of string list

let rec add_params_to_env env = function
    [] -> Result env
  | (x, p)::params ->
    begin
      match List.assoc_opt p env with
      | Some p_env -> add_params_to_env (update p (x::p_env) env) params
      | None -> Error(["Principal " ^ p ^ " not defined"])
    end

(* Checks if function: exist, right number of args, if data func, return list of errors *)
let check_func f args is_pattern funs =
  match List.assoc_opt f funs with
    | None -> [f ^ " not defined"]
    | Some((targs, tres, data_fun)) ->
      let n_args = List.length targs in
      if List.length args <> n_args then
      ["Wrong number of parameters in " ^ f] (* Types.show_term (Func(f,args)) instead of f *)
      else [] @
      if is_pattern && not data_fun then [f ^ " is not a data function"] else []

(* Checks if term: exists, check_func, return list of errors *)
let rec check_term (env: ident list) (funs: (ident * (data_type list * data_type * bool)) list) : term -> string list = function
  | Var(x) -> if List.mem x env then [] (* List.mem x -> if x exists = true *)
              else [x ^ " not defined"]
  | Func(f, args) ->
    check_func f args false funs @
    List.concat (List.map (check_term env funs) args)
  | Form(f, args) -> [] (* TODO Format typechecking *)
  | Tuple(l) ->
      List.concat(List.map (check_term env funs) l) (* recursively checks terms with their env and funcs, concat = flattens map *)
  | Eq(t1, t2) | And(t1, t2) | Or(t1, t2) ->
      check_term env funs t1 @ check_term env funs t2
  | Not(t) ->
      check_term env funs t

(* Checks if pattern: is not pre-defined, check_term, check_func, return list of errors *)
let rec check_pattern env funs = function
  | PVar(x) -> if not (List.mem x env) then []   (* check for free variables *)
               else [x ^ " already defined in pattern"]
  | PMatch(t) ->
      check_term env funs t
  | PForm(f, args) -> [] (* TODO Format typechecking *) 
  | PFunc(f, args) ->
    check_func f args true funs @
      List.concat (List.map (check_pattern env funs) args)
  | PTuple(l) ->
      List.concat(List.map (check_pattern env funs) l)

let rec typecheck (pr:problem): unit = 
  let p' = ("Dishonest", false)::pr.principals in
  let e = List.map (fun (p, x) -> p, initial_knowledge p [] pr.knowledge) pr.principals in
  let messages = check pr.protocol e [] pr.functions in
  List.iter (fun (txt, ty) ->
    Printf.printf "Error: %s at %s" txt (show_global_type_nr ty)) messages
(* Checks global types, return list of errors *)
and check
  (g : global_type)                             (* Global type *)
  (env : tenv)                                  (* each princ. with their known var, as a list *)
  (def : (ident * ((ident * principal) list * global_type)) list)   (* function name, it's env and the global type *)
  (funs : (ident * (data_type list * data_type * bool)) list)          (* function name, number of args, data type *)
  : (string * global_type) list                 (* error messages and where in code *)
   =

(* checks send: if p and q exist, if t is well-formed, updates env of q with x *)
match g with
| Send(p, q, x, t, g') ->
begin
  match List.assoc_opt p env with                  (* returns ident list of p in env *)
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    List.map (fun e -> (e, g)) (check_term env_p funs t) @ (* fun x.. : for return type error (message, G) *)
    match List.assoc_opt q env with
    | None -> ["Principal " ^ q ^ " not defined", g]
    | Some(env_q) ->
      let env' = update q (x::env_q) env in
      check g' env' def funs
end

(* checks branch: if p and q exist, if t is well-formed, recursively check patterns *)
| Branch(p, q, t, args) ->
begin
  match List.assoc_opt p env with
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    List.map (fun e -> (e, g)) (check_term env_p funs t) @
    match List.assoc_opt q env with
    | None -> ["Principal " ^ q ^ " not defined", g]
    | Some(env_q) ->
      List.concat (List.map (
        fun (p, g) -> check g env def funs @ List.map (fun e -> (e, g)) (check_pattern env_q funs p)   (* pattern and global type *)
        ) args)
end

(* Checks let-binding: update env of participant *)
| Compute(p, lb, g') ->
begin
  match List.assoc_opt p env with
  | None -> ["Principal " ^ p ^ " not defined", g]
  | Some(env_p) ->
    let rec let_bind env_p env = function
      | New(x, data_type, lb) -> (* TODO Implement check for data type *)
        let env_p' = (x::env_p) in
        let_bind env_p' (update p env_p' env) lb
      | Let(pat, t, lb) ->
        List.map (fun e -> (e, g)) (check_term env_p funs t) @
        List.map (fun e -> (e, g)) (check_pattern env_p funs pat) @
        let env_p' = binds pat @ env_p in
        let_bind env_p' (update p env_p' env) lb
      | Event(name, ts, lb) ->
        List.map (fun e -> (e, g)) (List.concat (List.map (check_term env_p funs) ts)) @
        let_bind env_p env lb
      | LetEnd -> check g' env def funs
    in let_bind env_p env lb
end

(* Checks function definition: *)
| DefGlobal(f, params, g', g'') ->
  let def' = ((f, (params, g'))::def) in
  let env' = add_params_to_env env params in
  begin
    match env' with
      | Error(err) -> List.map (fun e -> (e, g)) err
      | Result(env_param) -> (check g' env_param def' funs) @ (* obs recursion on def' *)
                       (check g'' env def' funs)
  end

(* Checks function calls *)
| CallGlobal(f, args) ->
  begin
    match List.assoc_opt f def with
      | None -> ["Funcion " ^ f ^ " not declared in ", g]
      | Some(params, g) ->
        if List.length args <> List.length params then
          ["Arguments and parameter lengths do not match in ", g]
        else
          List.map (fun e -> (e, g)) (List.concat (List.map (fun (t, (x, p)) -> check_term (List.assoc p env) funs t) (List.combine args params)))
  end

| GlobalEnd -> []
(*| _ -> [] *)
