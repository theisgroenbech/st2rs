open Rusttypes2
open Types
open Rustprinter

let indent = "    "
let abstract_traits = ":Serialize + DeserializeOwned"

let rec translateTerm = function
    Var(x) -> Id(ID(x))
  | Func(name, args) -> Exp(Id(ID(name)), translateArgs args)
  | Form(name, args) -> EStruct((ID(name)), StructValues(List.map (fun x-> StructValue x) ((List.map (fun a -> translateTerm a) args))))
  | Tuple(args) -> Id(ID("TUPLE"))
    (* show_term_pair args *)
(*  | Eq(t1, t2) -> show_term t1 ^ " = " ^ show_term t2
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

and fresh name data_type  =
  SDeclExp(DeclExp((ID(name)), toFunction (freshType data_type) (Ids([]))))

and process = function
    LSend(p, Form(fname, args), local_type) ->
    let send = toFunction "send" (Exps([Id(ID("c")); (toFunction "Repr::to_repr" (EStruct(ID(fname ), StructValues((List.map (fun a -> StructValue(translateTerm a)) args)))))])) in
    SDeclExp(DeclExp(translatePattern (PVar "c"), send))::process local_type
    (* process (LLet (PVar("c"), send, local_type)) *)
    (* ::local_type *)
  (* | LSend(p, t, local_type) -> process local_type *)
  | LNew (ident, data_type, local_type) -> (fresh ident data_type)::process local_type
  (* | LLet (PMatch(ident), term, local_type) -> *)
    (* indent ^ "if " ^ show_term ident ^ " != " ^ show_term term ^ " { panic!(\"" ^show_term ident ^ " does not match " ^ show_term term  ^"\") };\n" ^ process local_type *)
  | LLet (PForm(fname, args), term, local_type) -> SDeclExp(PatrExp(toStructPattern fname args, translateTerm term))::process local_type
  | LLet (PMatch(mat), term, local_type) ->
    (* let block = SDeclExp(DeclExp(translatePattern (PMatch(mat)), translateTerm term)) in *)
    [SIfStatement(If(OExp(translateTerm mat, Equals, translateTerm term), BStmts(process local_type)))]
  | LLet (ident, term, local_type) -> SDeclExp(DeclExp(translatePattern ident, translateTerm term))::process local_type
  (* | LRecv (principal, pattern, Form(fname, args), local_type) ->  SDeclExp(DeclExp(translatePattern pattern, translateTerm term))::process local_type *)
  | LRecv (principal, PVar(x), term, LLet (PForm(fname, args), Var(xx), local_type)) ->
    SDeclExp(DeclExp((ID("(c," ^x ^ ")")), toFunction "recv" (Id(ID("c")))))::SDeclExp(PatrExp(toStructPattern fname args, Id(ID("Repr::from_repr(" ^ xx ^ ")"))))::process local_type
  | LRecv (principal, PVar(x), term, local_type) ->  SDeclExp(DeclExp((ID(x)), toFunction "recv" (Id(ID("c")))))::process local_type
(* | LRecv (principal, PMatch(t), x, local_type) ->  SIfStatement(If(translateTerm t, BStmts([SDeclExp(DeclExp((ID("XX")), toFunction "recv" (Ids([]))))])) )::process local_type *)
(* | LRecv (principal, pattern, term, local_type) -> process local_type *)
  | LEvent (ident, term, local_type) -> process local_type
  | LLocalEnd -> [SExp(toFunction "close" (Id(ID("c"))))]
  | _ -> [End]
(* *)
and typedIds t =
  let rec inner dt i =
    match dt with
      [] -> []
    | x::xs -> TypedID(ID("a" ^(string_of_int i)), Custom(show_dtype x)) :: (inner xs (i+1)) in
  TypedIDs(inner t 1)

and abstract_types (t : data_type list) =
  String.concat "" (List.map (fun tt -> show_dtype tt) t)

and func f =
  match f with
    (name,(args,ret,bool,[])) -> Function(ID(name), typedIds(args), Custom(show_dtype ret), BStmts([SExp(Unimplemented)]))
  | (name,(args,ret,bool,a_types)) -> Function(ID(name ^ "<" ^ (abstract_types a_types ^ abstract_traits) ^ ">"), typedIds(args), Custom(show_dtype ret), BStmts([SExp(Unimplemented)]))

and functions (f : (ident * (data_type list * data_type * bool * data_type list)) list) =
  List.map (fun f -> func f) f

and format f =
  match f with
  | (name, data_types) -> Struct(ID(name), RTypes(List.map (fun d-> Custom(show_dtype d)) data_types))

and formats f =
  List.map (fun f -> format f) f

and rust_formats form =
  printStructs(formats form)

and rust_handwritten =
  printHandWritten

and rust_functions (f : (ident * (data_type list * data_type * bool * data_type list)) list) t =
  let freshTypeFunctions = List.map (fun (typ) -> (freshType typ, ([], typ, false, []))) t in
  printFunctions (functions (f @ freshTypeFunctions))


let rust_process principal proc = (printStatements (SFunction(Function(ID(String.lowercase principal),TypedIDs([TypedID(ID("c"), Custom("Chan<(), " ^ principal^">"))]), Empty,(BStmts(process proc))))))
