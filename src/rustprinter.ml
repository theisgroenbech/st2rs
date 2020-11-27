open Rusttypes2


let rec printStructPattern = function
      StructPattern(rId, args) -> printrId rId ^ "(" ^ String.concat "," (List.map (fun a -> printrId a) args) ^ ")"

and printStructValues = function
    StructValue(x) -> printExp x

and printStruct = function
    Struct(name, RTypes(types)) -> "struct " ^ printrId name ^ "(" ^ printTypes types ^ ");"

and printStructs structs = String.concat "\n" (List.map (fun s-> printStruct s) structs)

and printrId = function
      ID(s) -> s

and printExp = function
      Id(id) -> printrId id
    | Ids([]) -> ""
    | Ids(lst) -> "(" ^ String.concat "," (List.map (fun i-> printrId i) lst) ^ ")"
    | Ref(ref, exp) -> "&" ^ printExp exp
    | EStruct(id, StructValues(structValues)) -> printrId id ^ "(" ^ String.concat "," (List.map (fun x-> printStructValues x) structValues) ^ ")"
    | Exp(exp1, exp2) -> printExp exp1 ^ "(" ^ printExp exp2 ^ ")"
    | Exps(exps) -> String.concat "," (List.map (fun i-> printExp i) exps)
    | Unimplemented -> "unimplemented!()"


and printSDeclExp = function
      DeclExp (rId, exp) -> "let " ^ printrId rId ^ " = " ^ printExp exp
    | PatrExp (s, exp) -> "let " ^ printStructPattern s ^ " = " ^ printExp exp

and printBlock = function
      Empty -> "{}"
    | BStmts(lst) -> ("{\n") ^ String.concat (";\n") (List.map (fun s -> printStatements s) lst) ^ "}"
(* | BStmtsExp(stmt, exp) ->  *)

and printType = function
    U8 -> "u8"
  | Custom(s) -> s

and printTypes t =
  String.concat ", " (List.map (fun typ -> printType typ) t)

and printTypedId = function
    TypedID (id,typ) -> printrId id ^ ": " ^ printType typ

and printTypedIds t =
  String.concat ", " (List.map (fun typ -> printTypedId typ) t)

and printFunction = function
    Function(id, args, Empty, block) -> "fn " ^ printrId id ^ "(" ^ ") " ^ printBlock block
  | Function(id, TypedIDs(args), typ, block) -> "fn " ^ printrId id ^ "(" ^ printTypedIds args ^ ") -> "^ printType typ ^ " " ^ printBlock block

and printFunctions funs = String.concat "\n" (List.map (fun f-> printFunction f) funs)

and printStatements = function
      SDeclExp(declExp) -> printSDeclExp declExp
    | SBlock(block) -> printBlock block
    | SExp(exp) -> printExp exp
    | SFunction(rFunction) -> printFunction rFunction
    (* | SStruct(rStruct) -> "STRUCT" *)
    | End -> ""
