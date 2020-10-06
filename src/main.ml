open Lexer
open Lexing
open Printf
open Translation

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec print_errors = function
  | (msg, g)::err ->
    fprintf stderr "%s in %s\n" msg (Types.show_global_type_nr g);
    print_errors err
  | [] -> ()


let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some { name = name; principals = p; knowledge = k; protocol = g; types = t; functions = f; equations = eq; formats = form;} ->
     let type_check_f = ("fst", (1, false))::("snd", (1, false))::("pair", (2, false)):: (List.map (fun (id,(lst, rtn, bool)) ->  id,(List.length lst, bool)) f) in

    let env = List.map (fun (p, x) -> p, []) p in
    let errors = Typecheck.check g env [] type_check_f in
    let rules = translate name p k g t f eq form in
    ()
    (* print_errors errors; *)
    (* printf "%s\n" (Types.show_global_type g); *)
    (* printf "theory %s\nbegin\nfunctions: %s\nequations: %s\n\n%s\nend\n" name (Types.show_fdefs f) (Types.show_eqdefs eq) (Types.show_rules 1 rules) *)
  | None -> ()

let loop filename () =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  close_in inx

let main = loop (Sys.argv.(1)) ()
