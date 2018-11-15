%{
  open Types
%}

(* Tokens *)

%token <Span.t> INT_TYPE UNIT_TYPE TO
%token <Int.t> INT
%token <String.t> VAR
%token <Span.t> UNIT
%token <Span.t> TRUE FALSE
%token <Span.t> LT LE GE GT EQ NE
%token <Span.t> ADD SUB MUL DIV
%token <Span.t> LAND LOR
%token <Span.t> LAMBDA
%token <Span.t> DOT COMMA COLON
%token <Span.t> NOT
%token <Span.t> LPAREN RPAREN LBRACE RBRACE
%token <Span.t> INL INR CASE OF OR
%token <Span.t> IF THEN ELSE
%token <Span.t> EOF

(* Precedence and associativity *)

%left OR
%left AND
%left EQ NE
%left LT LE GE GT
%left ADD SUB
%left MUL DIV
%nonassoc LPAREN LBRACE
%left DOT

%start <Exp.t> program

%%

program:
| exp EOF { $1 }

exp:
| IF exp THEN exp ELSE exp    { (Exp.If($2, $4, $6), fst $6) }
| LPAREN exp COMMA exp RPAREN { (Exp.Prod($2, $4), $5) }
| exp DOT INT                 { (Exp.Proj($1, $3), fst $3) }
| VAR                         { (Exp.Var $1, fst $1) }


typ:
| INT_TYPE { (Type.Int, $1) }
