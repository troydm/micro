%{

open Ast

%}

%token BEGIN END EOF
%token <string> IDENTIFIER
%token <int> LITERAL
%token READ WRITE
%token ASSIGN 
%token LEFTPAREN RIGHTPAREN  
%token ADDOP SUBOP
%token COMMA SEMICOLON 

%start program
%type <Ast.ast> program

%%

program:
| BEGIN statements END EOF { PROGRAM $2 }
;

statements:
| { [] }
| statement SEMICOLON statements { $1 :: $3 }
;

statement:
| IDENTIFIER ASSIGN expression { ASSIGNMENT ($1, $3) }
| READ LEFTPAREN identifier_list RIGHTPAREN { READCALL $3 }
| WRITE LEFTPAREN expression_list RIGHTPAREN { WRITECALL $3 }
;

expression_list:
| expression { [$1] }
| expression COMMA expression_list { $1 :: $3 }
;

identifier_list:
| IDENTIFIER { [$1] } 
| IDENTIFIER COMMA identifier_list { $1 :: $3 }
;

expression:
| IDENTIFIER { VAR $1 }
| LITERAL { NUMBER $1 } 
| addop { $1 }
| subop { $1 }
;

addop:
| LITERAL ADDOP LITERAL { NUMBER ($1 + $3) }
| expression ADDOP expression { ADD ($1, $3) }
;

subop:
| LITERAL SUBOP LITERAL { NUMBER ($1 - $3) }
| expression SUBOP expression { SUB ($1, $3) }
;

%%
