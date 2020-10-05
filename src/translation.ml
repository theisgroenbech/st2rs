open Types
open Localtypes
open Rusttypes
(* global_type, ID, N, (prin -> Rule), (prin -> [VAR]), (ID -> [VAR, prin]) = return Rule*)
 let rec tr g f n r e df =
  match g with
  | Send(p, q, x, t, g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let Rule(b2, l2, e2, r2) = List.assoc q r in
     let env_p = List.assoc p e in
     let r' = update p (Rule([], [Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)], [], []))
                (update q (Rule((LetB(PVar x, t))::b2, Fact("In", [Var x])::l2, e2, r2)) r) in
     let e' = update q (x::List.assoc q e) e in
     Rule(b1, l1, e1, Fact("Out", [t])::Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)::r1) :: tr g' f (n+1) r' e' df
  | Branch(p, q, t, gs) ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let env_p = List.assoc p e in
     let r' = update p (Rule([], [Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)], [], [])) r in
     let rec tr_branch = function
       | [] -> []
       | (pat, g')::gs' ->
          let Rule(b2, l2, e2, r2) = List.assoc q r in
          let r'' = update q (Rule(b1, Fact("In", [pattern_to_term pat])::l2, e2, r2)) r' in
          let e' = update q (binds pat@List.assoc q e) e in
          tr g' f (n+1) r'' e' df @ tr_branch gs' in
     Rule(b1, l1, e1, Fact("Out", [t])::Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)::r1) :: tr_branch gs
  | Compute(p, New(x, letb), g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let r' = update p (Rule(b1, Fact("Fr", [Var x])::l1, e1, r1)) r in
     let e' = update p (x::List.assoc p e) e in
     tr (Compute(p, letb, g')) f n r' e' df
  | Compute(p, Let(pat, t, letb), g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let r' = update p (Rule(LetB(pat, t)::b1, l1, e1, r1)) r in
     let e' = update p (binds pat@List.assoc p e) e in
     tr (Compute(p, letb, g')) f n r' e' df
  | Compute(p, Event(name, args, letb), g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let r' = update p (Rule(b1, l1, Fact(name, args)::e1, r1)) r in
     tr (Compute(p, letb, g')) f n r' e df
  | Compute(p, LetEnd, g') ->
     tr g' f n r e df
  | DefGlobal(f', params, g1, g2) ->
     let e' = List.map (fun (p, env_p) ->
                  let rec params_p = function
                      [] -> []
                    | (x, p')::params' -> if p = p' then x::params_p params' else params_p params' in (p, params_p params @ env_p)) e in
     let r' = List.map (fun (p, _) ->
                  (p, Rule([], [Fact(f'^"_"^p^"_"^string_of_int 0, id_to_var (List.assoc p e'))], [], []))) r in
     let df' = (f', params)::df in
     tr g1 f' 1 r' e' df' @ tr g2 f n r e df'
  | CallGlobal(f', args) ->
     let params = List.assoc f' df in
     let pa = List.combine params args in
     List.map (function
           p, Rule(b, l, e, r) ->
           let p_args = pa |> List.filter (fun ((_, p'), _) -> p = p') |> List.map (fun ((_, _), t) -> t) in
           let p_env = [] in
           Rule(b, l, e, Fact(f'^"_"^p^"_"^string_of_int 0, p_args @ p_env)::r)
       ) r
  | GlobalEnd -> List.map (fun (p, rule) -> rule) r

(************************************************************)

  let rec initial_knowledge p e = function
    | [] -> e
    | (t', p') :: t ->
      if p' = p then initial_knowledge p (t'::e) t
      else initial_knowledge p e t

let init_rules e p =
  let rec dishonest e r n = function
    | [] -> r
    | (p, false) :: t -> dishonest e ((p, Rule([], [], [], []))::r) n t
    | (p, true) :: t ->
      let env_p = List.assoc p e in              (* [e1, e2, e3, e4, ..] *)
      let p' = prin_to_ident p in
      let env_p' = List.map(fun x -> [("$"^p');x]) env_p in

      if n = 0 then
        let init_rule = ("Dishonest", Rule([], [Fact("Fr", id_to_var ["~x"] )], [], [Fact("!Initial", id_to_var ["$P, ~x"])]))::r in
        dishonest e ((p, Rule([], (List.map (fun x -> Fact("!Initial", id_to_var x)) env_p'), [Fact("Dishonest", id_to_var [("$"^p')])], [Fact("Out", id_to_var env_p)] ))::init_rule) 1 t
      else
        dishonest e ((p, Rule([], (List.map (fun x -> Fact("!Initial", id_to_var x)) env_p'), [Fact("Dishonest", id_to_var [("$"^p')])], [Fact("Out", id_to_var env_p)] ))::r) 1 t
  in dishonest e [] 0 p

let toString (g : global_type) =
  show_global_type g

let local_type types principal =
  show_local_type (to_local_type types principal)

  let translate
    (name : ident)                            (* Name *)
    (p : (principal * bool) list)             (* Principals *)
    (k : (ident * principal) list)            (* Knowledge *)
    (g : global_type)                         (* Protocol *)
    (types: data_type list)                   (* Types *)
    (f : (ident * (int * bool)) list)         (* Functions *)
    (eq : (term * term) list)                 (* Equations *)
    : msr_rule list =

    let p' = ("Dishonest", false)::p in
    let e = List.map (fun (p, x) -> p, initial_knowledge p [] k) p' in
    let r = init_rules e p in
    (* Printf.printf "global_type: \n%s\n" (toString g); *)
    List.map (fun (p, b) -> Printf.printf "%s\n" (rust_channel p (to_local_type g p))) p;
    Printf.printf "%s\n" (rust_types types);
    Printf.printf "\n";
    Printf.printf "%s\n" (rust_functions f);
    Printf.printf "\n";
      List.map (fun (p, b) -> Printf.printf "%s\n\n" (rust_process p (to_local_type g p))) p;
    (* Printf.printf "\n"; *)
    (* List.map (fun (p, b) -> Printf.printf "%s:\n%s\n\n" p (local_type g p)) p; *)

    tr g name 0 r e []
