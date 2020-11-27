open Rusttypes2
open Types
open Rustprinter

let indent = "    "

let rec translateTerm = function
    Var(x) -> Id(ID(x))
  | Func(name, args) -> Exp(Id(ID(name)), translateArgs args)
  | Form(name, args) -> EStruct((ID(name)), StructValues(List.map (fun x-> StructValue x) ((List.map (fun a -> translateTerm a) args))))
                          (*| Tuple(args) -> show_term_pair args
  | Eq(t1, t2) -> show_term t1 ^ " = " ^ show_term t2
  | And(t1, t2) -> show_term t1 ^ " & " ^ show_term t2
  | Or(t1, t2) -> show_term t1 ^ " | " ^ show_term t2
  | Not(t) -> "~" ^ show_term t *)
  | _ -> Id(ID("TERM"))

and translatePattern = function
    PVar(x) -> ID(x)
  | PForm(fname, args) -> ID(fname)
  | _ -> ID("PATTERN")
    (* show_format fname ^ "(" ^ show_pattern_list args ^ ")" *)
(* letStructInstance = Exp *)
    (* | PFunc(name, args) -> name ^ "(" ^ pattern_list args ^ ")" *)
    (* | PTuple(args) -> "pair(" ^ pattern_list args ^ ")" *)
    (* | PMatch(t) -> "=" ^ term_as_type t *)

and translateArgs args =
  Exps( (List.map (fun a -> translateTerm a) args))
  (* Exp(List.map (fun a -> translateTerm a) args) *)

and toStructPattern fname args =
  StructPattern(ID(fname), List.map (fun a -> translatePattern (a)) args)

and toFunction name exp =
    Exp(Id(ID(name)),exp)

and freshType t =
    "fresh_" ^ show_dtype t

and fresh name data_type =
  SDeclExp(DeclExp((ID(name)), toFunction (freshType data_type) (Ids([]))))

and process = function
    LSend(p, Form(fname, args), local_type) -> SExp(toFunction "send" (EStruct(ID(fname), StructValues((List.map (fun a -> StructValue(translateTerm a))) args))))::process local_type
  (* | LSend(p, t, local_type) -> process local_type *)
  | LNew (ident, data_type, local_type) -> (fresh ident data_type)::process local_type
  (* | LLet (PMatch(ident), term, local_type) -> *)
    (* indent ^ "if " ^ show_term ident ^ " != " ^ show_term term ^ " { panic!(\"" ^show_term ident ^ " does not match " ^ show_term term  ^"\") };\n" ^ process local_type *)
  | LLet (PForm(fname, args), term, local_type) -> SDeclExp(PatrExp(toStructPattern fname args, translateTerm term))::process local_type
  | LLet (ident, term, local_type) -> SDeclExp(DeclExp(translatePattern ident, translateTerm term))::process local_type
  (* | LRecv (principal, pattern, Form(fname, args), local_type) ->  SDeclExp(DeclExp(translatePattern pattern, translateTerm term))::process local_type *)
  | LRecv (principal, PVar(x), term, local_type) ->  SDeclExp(DeclExp((ID(x)), toFunction "recv" (Ids([]))))::process local_type
(* | LRecv (principal, pattern, term, local_type) -> process local_type *)
  | LLocalEnd -> [End]
  | LEvent (ident, term, local_type) -> process local_type
  (* | _ -> [End] *)

and typedIds t =
  let rec inner dt i =
    match dt with
      [] -> []
    | x::xs -> TypedID(ID("a" ^(string_of_int i)), Custom(show_dtype x)) :: (inner xs (i+1)) in
  TypedIDs(inner t 1)

and func = function
    (name,(args,ret,bool)) -> Function(ID(name), typedIds(args), Custom(show_dtype ret), BStmts([SExp(Unimplemented)]))

and functions (f : (ident * (data_type list * data_type * bool)) list) =
  List.map (fun f -> func f) f

and format f =
  match f with
  | (name, data_types) -> Struct(ID(name), RTypes(List.map (fun d-> Custom(show_dtype d)) data_types))

and formats f =
  List.map (fun f -> format f) f

and rust_formats form =
  printStructs(formats form)

and rust_functions (f : (ident * (data_type list * data_type * bool)) list) t =
  let freshTypeFunctions = List.map (fun (typ) -> (freshType typ, ([], typ, false))) t in
  printFunctions (functions (f @ freshTypeFunctions))


let rust_process principal proc = (printStatements (SFunction(Function(ID(String.lowercase principal),TypedIDs([]), Empty,(BStmts(process proc))))))
