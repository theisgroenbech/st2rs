open Types
open Localtypes

let rec show_params = function
  [] -> ""
  | [t] -> "" ^ (show_dtype t)
  | (t::ts) -> "" ^ (show_dtype t) ^ ", " ^ show_params ts

and show_function = function
  (f, (args_t, ret, _, _)) -> "fun " ^ f ^ "(" ^ (show_params args_t) ^ "): " ^ (show_dtype ret)

and build_function = function
    (f, (args_t, _, _, _)) -> (f, List.map (fun t -> show_dtype t) args_t)

let rec build_equation_params t pos function_types names_and_types = (* [(var name, type)...] *)
  match (t, function_types) with
  | (Var(_), []) -> []
  | (Var(x), _) -> [(x, List.nth function_types pos)]
  | (Func(name, args), _) -> List.flatten (List.mapi (fun i arg -> build_equation_params arg i (List.assoc name names_and_types) names_and_types) args)
  | _ -> []

and show_equation_params equation names_and_types = 
  match equation with
  | (t1, t2) -> String.concat ", " (
      List.map (fun (a,b) -> a ^ ": " ^ b) 
        (List.sort_uniq (fun (a,_) (c,_) -> compare a c) (List.merge (fun _ _ -> 0) (build_equation_params t1 0 [] names_and_types) (build_equation_params t2 0 [] names_and_types))))

and show_equation equation names_and_types =
  match equation with
  | (t1, t2) -> "equation forall " ^ (show_equation_params equation names_and_types) ^ ";\n\t" ^ (show_term t1) ^ " = " ^ (show_term t2)

and show_local_type = function
    LSend(p, t, local_type) -> "\tout(c, " ^ show_term t  ^");\n"^ show_local_type local_type
  | LNew (ident, data_type, local_type) -> "\tnew " ^ ident ^ ": " ^ show_dtype data_type ^ ";\n" ^ show_local_type local_type
  | LLet (ident, term, local_type) -> "\tlet " ^ show_pattern ident ^ " = " ^ show_term term ^ " in\n" ^ show_local_type local_type
  | LRecv (principal, pattern, term, local_type) -> "\tin(c, " ^ show_pattern pattern ^ ": bitstring);\n" ^ show_local_type local_type
  | LEvent (ident, termlist, local_type) -> "\tevent " ^ ident ^ "(" ^ show_term_list termlist ^ ");\n" ^ show_local_type local_type
  | LLocalEnd -> "\t0."

and show_format = function
  (name, types) -> "fun " ^ name ^ "(" ^ (String.concat ", " (List.map (fun t -> show_dtype t) types)) ^ "): bitstring [data]."

and show_env = function
  env ->
    let vars = List.flatten (List.map (fun (_, e) -> e) env) in
    let uniq_vars = List.sort_uniq (fun (a,_) (c,_) -> compare a c) vars in
    String.concat "\n" (List.map (fun (name, dtype) -> "\tnew " ^ name ^ ": " ^ show_dtype dtype ^ ";") uniq_vars)

and show_party_params = function
  params -> String.concat "" (List.map (fun (name, dtype) -> ", " ^ name ^ ": " ^ show_dtype dtype) params)

and instantiate_party_process_vars party = function
  env -> String.concat "" (List.map (fun (i, _) -> ", " ^ i) (List.assoc party env))

let proverif (pr:problem): unit =
  let env = List.map (fun (p, x) -> p, initial_knowledge p [] pr.knowledge) pr.principals in
  Printf.printf  "(* Protocol: %s *)\n\n" pr.name;
  let function_types = List.map (fun f -> build_function f) pr.functions in
  List.iter (fun t -> 
    Printf.printf "type %s.\n" (show_dtype t)) pr.types;
  Printf.printf "%s\n" "";
  List.iter (fun f -> 
    Printf.printf "%s\n" (show_format f)) pr.formats;
  Printf.printf "%s\n" "";
  List.iter (fun t -> 
    Printf.printf "%s.\n" (show_function t)) pr.functions;
  Printf.printf "%s\n" "";
  List.iter (fun e -> 
    Printf.printf "%s.\n" (show_equation e function_types)) pr.equations;
  Printf.printf "%s\n" "";
  List.iter (fun (p, b) -> Printf.printf "let %s(c: channel%s) = \n%s\n\n" p (show_party_params (List.assoc p env)) (show_local_type (to_local_type pr.protocol p))) pr.principals;
  Printf.printf "process (\n\tnew c: channel;\n%s\n\t%s\n)" (show_env env) (String.concat " | " (List.map (fun (p, _) -> p ^ "(c" ^ (instantiate_party_process_vars p env) ^ ")") pr.principals))