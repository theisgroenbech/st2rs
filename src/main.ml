open Lexer
open Lexing
open Printf
open Translation
open Rusttypes
open Typecheck
open Localtypes

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

let main = 
  let filename = Sys.argv.(1) in
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let action = if Array.length Sys.argv > 2 then Sys.argv.(2) else "translate" in
  let f = match action with
  | "tamarin" -> translate
  | "rust" -> rust_output
  | "typecheck" -> typecheck
  | "projection" -> projection
  | "mscgen" -> Types.mscgen
  | _ -> fun x -> ()
  in
  match parse_with_error lexbuf with
  | Some p -> f p
  | None -> ();
  close_in inx