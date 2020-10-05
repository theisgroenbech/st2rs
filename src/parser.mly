%{
  open Types
%}
%token <string> ID
%token <int> NUM
%token <string> STRING
%token COMMA COLON SEMI PCT ARROW AT
%token LEFT_PAR RIGHT_PAR LEFT_ANGLE RIGHT_ANGLE LEFT_BRACE RIGHT_BRACE LEFT_BRACK RIGHT_BRACK
%token EQ AND OR NOT DIV
%token NEW LET EVENT IN END MATCH WITH DATA
%token PROBLEM PRINCIPALS KNOWLEDGE FUNCTIONS EQUATIONS PROTOCOL DISHONEST LEMMA
%token EOF

%start <Types.problem option> program
%%

opt_knowledge:
| KNOWLEDGE; COLON; k = separated_list(COMMA, indef); SEMI; { k }
| { [] }

program:
| PROBLEM; COLON; n = ID; SEMI;
  PRINCIPALS; COLON; p = separated_list(COMMA, prindef); SEMI;
  k = opt_knowledge;
  FUNCTIONS; COLON; f = separated_list(COMMA, fundef); SEMI;
  EQUATIONS; COLON; e = separated_list(COMMA, eqdef); SEMI;
  PROTOCOL; COLON; g = global_type;
  l = opt_lemm; EOF
  (* Add Lemma as string *)
{ Some { name = n; principals = p; knowledge = k; functions = f; equations = e; protocol = g; lemm = l} };

fundef:
| f = ID; DIV; arity = NUM; LEFT_BRACE; DATA; RIGHT_BRACE { (f, (arity, true)) }
| f = ID; DIV; arity = NUM { (f, (arity, false)) }

eqdef:
| lhs = term; EQ; rhs = term { (lhs, rhs) }

indef:
| t = ID; AT; prin = ID { (t, prin) }

prindef:
| name = ID; LEFT_BRACK; DISHONEST; RIGHT_BRACK
    { name, true }
| name = ID
    { name, false }

(* Choose? *)
term:
| name = ID
  { Var(name) }
| name = ID; LEFT_PAR; args = term_list; RIGHT_PAR
  { Func(name, args) }
| LEFT_ANGLE; args = term_list; RIGHT_ANGLE
  { Tuple(args) }
| t1 = term; EQ; t2 = term
  { Eq(t1, t2) }
| t1 = term; AND; t2 = term
  { And(t1, t2) }
| t1 = term; OR; t2 = term
  { Or(t1, t2) }
| NOT; t = term
  { Not(t) }
| LEFT_PAR; t = term; RIGHT_PAR
  { t };

term_list:
| l = separated_list(COMMA, term)
  { l };

pattern:
| LEFT_PAR; p = pattern; RIGHT_PAR {p}
| name = ID
  { PVar(name) }
| PCT; t = term
  { PMatch(t) }
| name = ID; LEFT_PAR; pargs = pattern_list; RIGHT_PAR
  { PFunc(name, pargs) }
| LEFT_ANGLE; pargs = pattern_list; RIGHT_ANGLE
  { PTuple(pargs) }

pattern_list:
| l = separated_list(COMMA, pattern)
  { l };

let_bind:
| NEW; name = ID; SEMI; letb = let_bind
  { New(name, letb) }
| LET; p = pattern; EQ; t = term; SEMI; letb = let_bind
  { Let(p, t, letb) }
| EVENT; name = ID; LEFT_PAR; ts = term_list; RIGHT_PAR; SEMI; letb = let_bind
  { Event(name, ts, letb) }
| { LetEnd };

global_type:
| prin1 = ID; ARROW; prin2 = ID; COLON; p = pattern; gt = global_type
  { Branch(prin1, prin2, pattern_to_term p, [p, gt]) }
| prin1 = ID; ARROW; prin2 = ID; COLON; x = ID; EQ; t = term; gt = global_type
  { Send(prin1, prin2, x, t, gt ) }
| prin1 = ID; ARROW; prin2 = ID; COLON; MATCH; t1 = term; WITH; LEFT_BRACE; branches = branch_list; RIGHT_BRACE
  { Branch(prin1, prin2, t1, branches) }
| prin = ID; LEFT_BRACE; lb = let_bind; RIGHT_BRACE; gt = global_type
  { Compute(prin, lb, gt) }
| LET; name = ID; LEFT_PAR; params = separated_list(COMMA, param); RIGHT_PAR; EQ; gt1 = global_type; IN; gt2 = global_type
  { DefGlobal(name, params, gt1, gt2) }
| name = ID; LEFT_PAR; args = term_list; RIGHT_PAR
  { CallGlobal(name, args) }
| END
  { GlobalEnd };

param:
| x = ID; AT; p = ID { (x, p) }

branch_list:
| { [] }
| p = pattern; COLON; gt = global_type; branches = branch_list
  { ((p, gt)::branches) };

opt_lemm:
| LEMMA; COLON; s = STRING
    { Some s }
| { None }