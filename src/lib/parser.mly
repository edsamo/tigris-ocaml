// parser.mly

%{
  open Absyn
%}

%token <bool>          LOGIC
%token <int>           INTEGER
%token <string>        STRING
%token <float>         REAL
%token <Symbol.symbol> ID
%token                 IF THEN ELSE
%token                 WHILE DO BREAK
%token                 LET IN END
%token                 VAR
%token                 LPAREN "(" RPAREN ")"
%token                 COLON ":" COMMA "," SEMI ";"
%token                 PLUS "+" MINUS "-" TIMES "*" DIV "/" MOD "%" POW "^"
%token                 EQ "=" NE "<>"
%token                 LT "<" LE "<=" GT ">" GE ">="
%token                 AND "&" OR "|"
%token                 ASSIGN ":="
%token                 EOF

%start <Absyn.lexp> program

%right THEN ELSE DO IN
%nonassoc ASSIGN
%left "|"
%left "&"
%nonassoc "=" "<>" ">" ">=" LE LT
%left "+" "-"
%left "*" "/" "%"
%right "^"

%%

program:
| EOF {$loc, SeqExp []}

exp:
 | x=LOGIC              {$loc, BoolExp x}
 | x=INTEGER            {$loc, IntExp x}
 | x=REAL               {$loc, RealExp x}
 | x=STRING             {$loc, StringExp x}
 | v=var                {$loc, VarExp v}
 | MINUS e=exp          {$loc, MinExp (MinusOp, e)}
 | e=exp o=oper f=exp   {$loc, OpExp (o, e, f)}
 | v=var ":=" e=exp     {$loc, AssignExp (v, e)}
 | f=ID "(" e=exps ")"  {$loc, CallExp (f, e)}
 | IF e=exp THEN f=exp ELSE g=exp {$loc, IfExp (e, f, Some g)} 
 | IF e=exp THEN f= exp {$loc, IfExp (e, f, None)} 
 | WHILE e=exp DO f=exp {$loc, WhileExp (e, f)}
 | BREAK                {$loc, BreakExp}
 | END                  {$loc, EndExp}
 | LET d=decs IN e=exp  {$loc, LetExp (d, e)}
 | "(" e=expseq ")"     {$loc, SeqExp e}

%inline oper:
 | "+"                                   {PlusOp}
 | "-"                                   {MinusOp}
 | "*"                                   {TimesOp}
 | "/"                                   {DivOp}
 | "%"                                   {ModOp}
 | "^"                                   {PowOp}
 | "="                                   {EqOp}
 | "<>"                                  {NeOp}
 | "<"                                   {LtOp}
 | ">"                                   {GtOp}
 | "<="                                  {LeOp}
 | ">="                                  {GeOp}
 | "&"                                   {AndOp}
 | "|"                                   {OrOp}

exps:
 | es=separated_list(",", e=exp {e}) {es}

expseq:
 | esq=separated_list(";", exp) {esq}

decs:
 | l=list(dec) {l}

dec:
 | v = vardec {v}
 | v = nonempty_list(typedec) {$loc, MutualTypeDecs v}
 | v = nonempty_list(fundec) {$loc, MutualFunctionDecs v}

vardec:
 | VAR v=ID ":" t=ID ":=" e=exp {$loc, VarDec (v, Some ($loc(t), t), e)}
 | VAR v=ID ":=" e=exp {$loc, VarDec (v, None, e)}

var:
 | x=ID {$loc, SimpleVar x}

typedec:
 | TYPE v=ID ":=" t=typecons {$loc, (v, t)}

typecons:
 | i=ID {$loc, NameCons ds}

fundec:
 | FUNCTION v=ID "(" a=args ")" ":" t=ID ":=" e=exp {$loc, (v, a, Some ($loc, t), e)}
 | FUNCTION v=ID "(" a=args ")" ":=" e=exp          {$loc, (v, a, None, e)}

arg:
 | v=ID ":" t=ID {$loc, (v, t)}

args:
 | ls=separated_list(",", arg) {ls}