open Types


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
  | (t1, t2) -> "reduc forall " ^ (show_equation_params equation names_and_types) ^ ";\n\t" ^ (show_term t1) ^ " = " ^ (show_term t2)

let proverif (pr:problem): unit =
  Printf.printf  "(* Protocol: %s *)\n\n" pr.name;
  let function_types = List.map (fun f -> build_function f) pr.functions in
  List.iter (fun t -> 
    Printf.printf "type %s.\n" (show_dtype t)) pr.types;
  Printf.printf "%s\n" "";
  List.iter (fun t -> 
    Printf.printf "%s.\n" (show_function t)) pr.functions;
    Printf.printf "%s\n" "";
  List.iter (fun e -> 
    Printf.printf "%s.\n" (show_equation e function_types)) pr.equations