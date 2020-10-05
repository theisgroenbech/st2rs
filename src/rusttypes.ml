open Types

let abstract_type = "T"
let concrete_type = "/* unimplemented */"
let functions_variable = "f"
let pair = "pair"
let pair_function = functions_variable ^ "." ^ pair
let indent = "    "
let interface_impl_name = "Functions"
let rec show_term_pair = function
    [] -> ""
  | [x] -> "" ^ show_term x (* In all cases we want to lend the value *)
  | (x::xs) ->  pair_function ^ "("  ^  "&" ^show_term x ^ ", &" ^ show_term_pair xs ^ ")"

and show_term = function
    Var(x) -> x
  | Func(name, args) -> functions_variable ^ "." ^ name ^ "(" ^ show_term_list args ^ ")"
  | Tuple(args) -> show_term_pair args
  | Eq(t1, t2) -> show_term t1 ^ " = " ^ show_term t2
  | And(t1, t2) -> show_term t1 ^ " & " ^ show_term t2
  | Or(t1, t2) -> show_term t1 ^ " | " ^ show_term t2
  | Not(t) -> "~" ^ show_term t

(* List options: empty, single item, list *)
and show_term_list = function
    [] -> ""
  | [x] -> "&" ^ show_term x (* In all cases we want to lend the value *)
  | (x::xs) ->  "&" ^ show_term x ^ ", " ^ show_term_list xs

let rec term_as_type = function
    Var(x) -> abstract_type
  | Func(name, args) -> abstract_type (*name ^ "(" ^ get_term_as_type_channel_list args ^ ")"*)
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

and channels = function
    LSend(p, t, local_type) -> "Send<" ^ term_as_type t ^ ", " ^ channels local_type ^ ">"
  | LRecv (principal, pattern, term, local_type) -> "Recv<" ^ term_as_type term ^", "^ channels local_type ^ ">"
  | LNew (ident, local_type) -> channels local_type
  | LLet (ident, term, local_type) -> channels local_type
  | LEvent (ident, term, local_type) -> channels local_type
  | _ -> "Eps"

and createChannel env =
    let rec c i =
      match (List.exists ((=) ("c"^(string_of_int i))) env) with
        true -> c (i+1)
      | false -> "c"^(string_of_int i)
    in c 0

and createArguments i =
  let rec inner i =
    match i with
    0 -> []
    | x -> ("a"^(string_of_int x) ^ ": " ^ "&" ^ abstract_type)::(createArguments (x-1)) in
   List.rev (inner i)

and functions = function
  | [] -> []
  | (name,(arguments,bool)) :: tail -> ("fn " ^ name ^ "(" ^ (String.concat ", " ("&self"::createArguments arguments)) ^ ") -> " ^abstract_type) :: functions tail

and print ident term =
  "println!(\"" ^ ident ^ ": " ^ String.concat " " (List.map (fun t -> "{}") term) ^ "\", " ^ show_term_list term ^ ");\n"

and fresh t =
  "fresh_" ^t
and process = function
    LSend(p, t, local_type) -> indent ^ "let c = c.send(" ^ show_term t ^ ");\n" ^ process local_type
  | LNew (ident, local_type) -> indent ^ "let " ^ ident ^ " = " ^ "f." ^ fresh abstract_type ^ "();\n" ^ process local_type
  | LLet (PMatch(ident), term, local_type) ->
    indent ^ "if " ^ show_term ident ^ " != " ^ show_term term ^ " { panic!(\"" ^show_term ident ^ " does not match " ^ show_term term  ^"\") };\n" ^ process local_type
    (* indent ^ "let " ^ show_term ident ^ " = " ^ show_term term ^ ";\n"^ *)
  | LLet (ident, term, local_type) -> indent ^ "let " ^ show_pattern ident ^ " = " ^ show_term term ^ ";\n" ^ process local_type
  | LRecv (principal, pattern, term, local_type) ->  indent ^ "let (c, " ^ show_pattern pattern ^") = c.recv();\n" ^ process local_type
  | LLocalEnd -> indent ^ "c.close();"
  | LEvent (ident, term, local_type) -> indent ^ print ident term^ process local_type
  | _ -> "0."

and rust_process principal proc =
  "fn " ^ String.lowercase principal ^ "(c: Chan<(), " ^ principal ^"<" ^ abstract_type ^ ">" ^ ">, "^functions_variable ^ ": &impl Interface<"^abstract_type^">) {\n" ^ process proc ^"\n}"

and print_type t =
  match t with
  | DType dtype -> dtype

and rust_types type_list =
  let types = List.map (fun t -> "type "^ print_type t ^ " = " ^ concrete_type ^ ";") type_list in
  String.concat "\n" (types)

and rust_interface f =
  let freshTypeFunctions = List.map (fun (typ) -> (fresh typ, (0,false))) [abstract_type] in
  "trait Interface<"^abstract_type^"> {\n" ^ indent ^ String.concat (";\n" ^indent) (functions (f @ freshTypeFunctions)) ^ ";\n}"

and rust_impl_interface f =
  let freshTypeFunctions = List.map (fun (typ) -> (fresh typ, (0,false))) [abstract_type] in
  "impl Interface<"^abstract_type^"> for "^ interface_impl_name ^" {\n" ^ indent ^ String.concat (" { unimplemented!() }\n" ^indent) (functions (f @ freshTypeFunctions)) ^ "{ unimplemented!() }\n}"

and rust_functions f =
  rust_interface f ^ "\n\n" ^ "struct Functions {}\n\n" ^ rust_impl_interface f

and rust_channel p t =
  "type " ^ p ^ "<" ^ abstract_type ^ ">" ^ " = " ^ channels t ^ ";"
