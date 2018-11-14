%{
  open Lambda
%}

%token <string> ID
%token LPAREN RPAREN LAMBDA DOT
%token EOF

%start main
%type <Lambda.t> main

%%

main:
  abs EOF             { $1 }
;

abs:
    LAMBDA ids { $2 }
  | app        { $1 }
  ;

ids:
    ID DOT app { Abs ($1, $3) }
  | ID ids     { Abs ($1, $2) }
  ;

app:
    app var { App ($1, $2) }
  | var     { $1 }
  ;

var:
    ID                { Var $1 }
  | LPAREN abs RPAREN { $2 }
  ;
