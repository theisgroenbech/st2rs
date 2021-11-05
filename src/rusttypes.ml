open Types
open Localtypes
open Rusttranslator

let abstract_type = "T"
let concrete_type = "/* unimplemented */"
let functions_variable = "f"
let pair = "pair"
let pair_function = functions_variable ^ "." ^ pair
let indent = "    "
let interface_impl_name = "Functions"
let enum = "I"
let enum_func =  "::"^enum

let rec show_term_pair = function
    [] -> ""
  | [x] -> "" ^ show_term x (* In all cases we want to lend the value *)
  | (x::xs) ->  pair_function ^ "("  ^  "&" ^show_term x ^ ", &" ^ show_term_pair xs ^ ")"

and show_term = function
    Var(x) -> x
  | Func(name, args) -> functions_variable ^ "." ^ name ^ "(" ^ show_term_list args ^ ")"
  | Form(name, args) -> show_format name ^ "(" ^ show_term_list_without_lending args ^ ")"
  | Tuple(args) -> show_term_pair args
  | Eq(t1, t2) -> show_term t1 ^ " = " ^ show_term t2
  | And(t1, t2) -> show_term t1 ^ " & " ^ show_term t2
  | Or(t1, t2) -> show_term t1 ^ " | " ^ show_term t2
  | Not(t) -> "~" ^ show_term t

and show_format name = name ^enum_func
(* List options: empty, single item, list *)
and show_term_list_without_lending = function
    [] -> ""
  | [x] -> "" ^ show_term x (* In all cases we want to lend the value *)
  | (x::xs) ->  "" ^ show_term x ^ ", " ^ show_term_list xs

and show_term_list = function
    [] -> ""
  | [x] -> "&" ^ show_term x (* In all cases we want to lend the value *)
  | (x::xs) ->  "&" ^ show_term x ^ ", " ^ show_term_list xs

let rec term_as_type = function
    Var(x) -> abstract_type
  | Func(name, args) -> abstract_type (*name ^ "(" ^ get_term_as_type_channel_list args ^ ")"*)
  | Form(name, args) -> name
  | Tuple(args) -> "(" ^ term_as_type_list args ^ ")"

and term_as_type_list = function
    [] -> ""
  | [x] -> term_as_type x
  | (x::xs) -> abstract_type ^ ", " ^ term_as_type_list xs

and pattern = function
    PVar(x) -> abstract_type
  | PFunc(name, args) -> name ^ "(" ^ pattern_list args ^ ")"
  | PTuple(args) -> "pair(" ^ pattern_list args ^ ")"
  | PMatch(t) -> "=" ^ term_as_type t

and pattern_list = function
    [] -> ""
  | [x] -> pattern x
  | (x::xs) -> pattern x ^ ", " ^ pattern_list xs

  and show_pattern = function
      PVar(x) -> x
    | PFunc(name, args) -> name ^ "(" ^ show_pattern_list args ^ ")"
    | PForm(fname, args) -> show_format fname ^ "(" ^ show_pattern_list args ^ ")"
    | PTuple(args) -> "<" ^ show_pattern_list args ^ ">"
    | PMatch(t) -> "=" ^ show_term t

  and show_pattern_list = function
      [] -> ""
    | [x] -> show_pattern x
    | (x::xs) -> show_pattern x ^ ", " ^ show_pattern_list xs

and channels = function
    LSend(_, opt, t, local_type) -> "Send<Repr<" ^ term_as_type t ^ ">, " ^ channels local_type ^ ">"
  | LRecv (_, opt, pattern, term, local_type) -> "Recv<Repr<" ^ term_as_type term ^">, "^ channels local_type ^ ">"
  | LNew (ident, data_type, local_type) -> channels local_type
  | LLet (ident, term, local_type) -> channels local_type
  | LEvent (ident, term, local_type) -> channels local_type
  | _ -> "Eps"

and createArguments (t:data_type list) =
  let rec inner dt i =
    match dt with
      [] -> []
    | x::xs -> ("a" ^(string_of_int i)^ ": " ^ "&" ^ show_dtype x)::(inner xs (i+1)) in
  inner t 1

and functions (f : (ident * (data_type list * data_type * bool)) list) =
    match f with
  | [] -> []
  | (name,(args,ret,bool)) :: tail -> ("fn " ^ name ^ "(" ^ (String.concat ", " ("&self"::createArguments args)) ^ ") -> " ^ show_dtype ret) :: functions tail

and print ident term =
  "println!(\"" ^ ident ^ ": " ^ String.concat " " (List.map (fun t -> "{}") term) ^ "\", " ^ show_term_list term ^ ");\n"

and fresh t =
  "fresh_" ^ show_dtype t

and process = function
    LSend(_, opt, t, local_type) -> indent ^ "let c = c.send(" ^ show_term t ^ ");\n" ^ process local_type
  | LNew (ident, data_type, local_type) -> indent ^ "let " ^ ident ^ " = " ^ "f." ^ fresh data_type ^ "();\n" ^ process local_type
  | LLet (PMatch(ident), term, local_type) ->
    indent ^ "if " ^ show_term ident ^ " != " ^ show_term term ^ " { panic!(\"" ^show_term ident ^ " does not match " ^ show_term term  ^"\") };\n" ^ process local_type
  | LLet (ident, term, local_type) -> indent ^ "let " ^ show_pattern ident ^ " = " ^ show_term term ^ ";\n" ^ process local_type
  | LRecv (_, opt, pattern, term, local_type) ->  indent ^ "let (c, " ^ show_pattern pattern ^") = c.recv();\n" ^ process local_type
  | LLocalEnd -> indent ^ "c.close();"
  | LEvent (ident, term, local_type) -> indent ^ print ident term^ process local_type
  | _ -> "0."

(* and rust_process principal proc = *)
  (* "fn " ^ String.lowercase principal ^ "(c: Chan<(), " ^ principal ^ ">, "^functions_variable ^ ": &impl Interface" ^") {\n" ^ process proc ^"\n}" *)

and print_type t =
  match t with
  | DType dtype -> dtype
  | DAType (dtype,_) -> dtype

and rust_types type_list =
  let types = List.map (fun t -> "type "^ print_type t ^ " = " ^ concrete_type ^ ";") type_list in
  String.concat "\n" (types)

and rust_a_types type_list =
  let types = List.map (function DAType(s1,s2) -> "#[derive(Serialize, Deserialize)]\npub struct " ^ s1 ^ "<" ^ s2 ^">(Vec<u8>, PhantomData<T>);") type_list in
  String.concat "\n" (types)


and rust_interface (f : (ident * (data_type list * data_type * bool)) list) t =
  let freshTypeFunctions = List.map (fun (typ) -> (fresh typ, ([], typ, false))) t in
  "trait Interface {\n" ^ indent ^ String.concat (";\n" ^indent) (functions (f @ freshTypeFunctions)) ^ ";\n}"

and rust_impl_interface (f : (ident * (data_type list * data_type * bool)) list) t =
  let freshTypeFunctions = List.map (fun (typ) -> (fresh typ, ([], typ, false))) t in
  "impl Interface for "^ interface_impl_name ^" {\n" ^ indent ^ String.concat (" { unimplemented!() }\n" ^indent) (functions (f @ freshTypeFunctions)) ^ "{ unimplemented!() }\n}"

and print_format f =
  match f with
  | (name, data_types) -> "enum " ^  name ^ " { "^ enum ^"(" ^ String.concat ", " (List.map (fun data -> print_type data) data_types) ^ ") }"


and rust_channel p t =
  "type " ^ p ^ " = " ^ channels t ^ ";"


let rust_output (pr:problem) : unit =
  Printf.printf "%s\n" (rust_handwritten);
  List.map (fun (p, b) ->
      Printf.printf "%s\n" (rust_channel p (to_local_type pr.protocol p))) pr.principals;
  let abstract_types = List.filter_map (function DAType(s1,s2) -> Some(DAType(s1,s2)) | _ -> None) pr.types in
  let concrete_types = List.filter_map (function DType(s1) -> Some(DType(s1)) | _ -> None) pr.types in
  Printf.printf "\n%s\n" (rust_a_types abstract_types);
  Printf.printf "\n%s\n" (rust_types concrete_types);
  Printf.printf "\n%s\n" (rust_formats pr.formats);
  Printf.printf "\n%s\n" (rust_functions pr.functions concrete_types);
  List.iter (fun (p, b) -> Printf.printf "\n%s\n" (rust_process p (to_local_type pr.protocol p))) pr.principals;
