open Types

let rec show_term = function
    Var(x) -> x
  | Func(name, args) -> name ^ "(" ^ show_term_list args ^ ")"
  | Form(name, args) -> name ^ "(" ^ show_term_list args ^ ")"
  | Tuple(args) -> "<" ^ show_term_list args ^ ">"
  | Eq(t1, t2) -> show_term t1 ^ " = " ^ show_term t2
  | And(t1, t2) -> show_term t1 ^ " & " ^ show_term t2
  | Or(t1, t2) -> show_term t1 ^ " | " ^ show_term t2
  | Not(t) -> "~" ^ show_term t

(* List options: empty, single item, list *)
and show_term_list = function
    [] -> ""
  | [x] -> show_term x
  | (x::xs) -> show_term x ^ ", " ^ show_term_list xs

and show_pattern = function
    PVar(x) -> x
  | PFunc(name, args) -> name ^ "(" ^ show_pattern_list args ^ ")"
  | PForm(name, args) -> name ^ "(" ^ show_pattern_list args ^ ")"
  | PTuple(args) -> "<" ^ show_pattern_list args ^ ">"
  | PMatch(t) -> "=" ^ show_term t

and show_pattern_list = function
    [] -> ""
  | [x] -> show_pattern x
  | (x::xs) -> show_pattern x ^ ", " ^ show_pattern_list xs

and show_let_bind = function
    New(name, data_type, letb) -> "  " ^ "new " ^ name ^ ";\n" ^ show_let_bind letb
  | Let(p, t, letb) -> "let " ^ show_pattern p ^ " = " ^ show_term t ^ " in\n" ^ show_let_bind letb
  | Event(ident, termList, let_bind) -> "EVENT"
  | LetEnd -> ""

and local_let_bind types g =
  match types with
      New(name, data_type, letb) -> LNew(name, data_type, local_let_bind letb g)
    | Let(p, t, letb) -> LLet(p, t, local_let_bind letb g)
    | LetEnd -> g
    | If(cond, ifl, LetEnd, letb) -> LIf(cond, local_let_bind ifl g, LLocalEnd, local_let_bind letb g) 
    | If(cond, ifl, ifr, letb) -> LIf(cond, local_let_bind ifl g, local_let_bind ifr g, local_let_bind letb g) 
    | Event (ident, terms, letb) -> LEvent(ident, terms, local_let_bind letb g)


(* Show global types *)
and show_global_type2 = function
    Send(p, q, _, x, t, g) -> "SEND: " ^ "from (" ^ p ^ ") to (" ^ q ^ "): name: " ^ x ^ " = " ^ show_term t ^ "\n" ^ show_global_type2 g
  | Branch(p, q, _, t, branches) ->
    "MATCH"^ p ^ "->" ^ q ^ ": match " ^ show_term t ^ " with {\n" ^ show_branches branches ^ "}\n"
  | Compute(p, letb, g) ->
    p ^ " {\n" ^ show_let_bind letb ^ "}\n" ^ show_global_type2 g
  | DefGlobal(name, params, g, g') -> show_global_type2 g
  | CallGlobal(name, params) -> ""
  | GlobalEnd ->
    ""

(* This takes the part of global where the participants do stuff before running *)
and unwrapGlobal global local =
  match global with
  | Compute(p, letb, g) -> Compute(p, letb, (unwrapGlobal g local))
  | GlobalEnd -> local
  | CallGlobal (_,_) -> local


and to_local_type global_type participant =
  match global_type with
    Send(sender, receiver, opt, x, t, g) when participant = sender -> LSend((if receiver < sender then receiver ^ sender else sender ^ receiver), opt, t, to_local_type g participant)
  | Send(sender, receiver, opt, x, t, g) when participant = receiver -> LRecv((if receiver < sender then receiver ^ sender else sender ^ receiver), opt, PVar(x), t, to_local_type g participant)
  | Send(_, _, _, _, _, g) -> to_local_type g participant
  | Compute(p, letb, g) when participant = p -> local_let_bind letb (to_local_type g participant)
  | Compute(p, letb, g) -> (to_local_type g participant)
  | DefGlobal(name, params, g, g') -> to_local_type (unwrapGlobal g' g) participant
  | _ -> LLocalEnd

and show_local_type local =
  match local with
    LSend(p, opt, t, local_type) -> "out(" ^ show_term t  ^") \n"^ show_local_type local_type
  | LNew (ident, data_type, local_type) -> "new " ^ ident ^ " : " ^ show_dtype data_type ^ ";\n" ^ show_local_type local_type
  | LLet (ident, term, local_type) -> "let " ^ show_pattern ident ^ " = " ^ show_term term ^ " in\n" ^ show_local_type local_type
  | LRecv (principal, opt, pattern, term, local_type) -> "let "^ show_pattern pattern  ^" = in() \n"^ show_local_type local_type
  | LEvent (ident, termlist, local_type) -> "event " ^ ident ^ "(" ^ show_term_list termlist ^ ");\n" ^ show_local_type local_type
  | LLocalEnd -> "0."


and show_global_type_nr = function
    Send(p, q, opt, x, t, g) -> p ^ show_channel_option opt ^ q ^ ": " ^ x ^ " = " ^ show_term t ^ " ..."
  | Branch(p, q, opt, t, branches) ->
    p ^ show_channel_option opt ^ q ^ ": match " ^ show_term t ^ " with {\n" ^ show_branches_nr branches ^ "}\n"
  | Compute(p, letb, g) ->
    p ^ " {\n" ^ show_let_bind letb ^ "}...\n"
  | DefGlobal(name, params, g, g') ->
    name ^ "("^show_params params^")" ^ show_global_type g ^ "\nin...\n"
  | CallGlobal(name, params) ->
    name ^ "(" ^ show_term_list params ^ ")"
  | GlobalEnd -> "end\n"

and show_branches = function
    [] -> ""
  | ((p, g)::branches) ->
    show_pattern p ^ ": " ^ show_global_type g ^ "\n" ^ show_branches branches

and show_branches_nr = function
    [] -> ""
  | ((p, g)::branches) ->
    show_pattern p ^ ": ...\n" ^ show_branches_nr branches

and show_params = function
    [] -> ""
  | [(x, p)] -> x ^ " @ " ^ p
  | ((x, p)::xs) -> x ^ " @ " ^ p ^ ", " ^ show_params xs

let projection (pr:problem): unit =
  Printf.printf  "%s" (show_global_type_nr pr.protocol);
  List.iter (fun (p, b) ->
    Printf.printf "let %s = %s\n" p (show_local_type (to_local_type pr.protocol p))) pr.principals
