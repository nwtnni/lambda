%{
  open Types
%}

(* Tokens *)

%token <Types.Int.t> INT
%token <Types.Var.t> VAR
%token <Span.t> UNIT
%token <Span.t> TRUE FALSE
%token <Span.t> LT LE GE GT                           (* int -> int -> bool *)
%token <Span.t> EQ NE                                 (* ∀t. t -> t -> bool *)
%token <Span.t> ADD SUB MUL DIV                       (* int -> int -> int  *)
%token <Span.t> LAND LOR NOT                          (* bool -> bool -> bool *)
%token <Span.t> LAMBDA DOT                            (* Abstraction *)
%token <Span.t> INT_TYPE BOOL_TYPE UNIT_TYPE TO COLON (* Types *)
%token <Span.t> LPAREN RPAREN COMMA PIL PIR           (* Products *)
%token <Span.t> INL INR LBRACE RBRACE CASE OF OR      (* Sums *)
%token <Span.t> IF THEN ELSE                          (* If statements *)
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

%start <Types.Exp.t> program

%%

program:
| exp EOF { $1 }

exp:
| IF exp THEN exp ELSE exp    { (Exp.If($2, $4, $6), snd $6) }
| LPAREN exp COMMA exp RPAREN { (Exp.Prod($2, $4), $5) }
| exp PIL                     { (Exp.Pil($1), $2) }
| exp PIR                     { (Exp.Pir($1), $2) }
| VAR                         { (Exp.Var($1), snd $1) }


typ:
| INT_TYPE { (Type.Int, $1) }
