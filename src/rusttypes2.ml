type rId =
    ID of string

type rType =
    I8
  | U8
  | I16
  | U16
  | I32
  | U32
  | I64
  | U64
  | F32
  | F64
  | Isize
  | Usize
  | Str
  | Boolean
  | Custom of string
  | Empty

type rTypes =
    RTypes of rType list

type typedId =
    TypedID of rId * rType

type typedIds =
    TypedIDs of typedId list

type declExp =
    DeclExp of rId * exp
  | PatrExp of rStructPattern * exp

and op =
    Equals
  | Or
  | And

and exp =
    Exps of exp list
  | Id of rId
  | Ids of rId list
  | Ref of ref * exp
  | EStruct of rId * structValues
  | Exp of exp * exp
  | OExp of exp * op * exp
  | Unimplemented

and ifStatement =
  | If of exp * block

and exps =
    Exps of exp list

and block =
    Empty
  | BStmts of stmts list
  | BStmtsExp of stmts * exp

and ref =
    Ref

and structValue =
    StructValue of exp

and structValues =
    StructValues of structValue list

and rStruct =
    Struct of rId * rTypes

and rStructPattern =
    StructPattern of rId * (rId list)

and structInstance  =
    StructInstance of rId * structValues

and rFunction =
    Function of rId * typedIds * rType * block

and stmts =
    SDeclExp of declExp
  | SBlock of block
  | SExp of exp
  | SIfStatement of ifStatement
  | SFunction of rFunction
  | SStruct of rStruct
  | End
