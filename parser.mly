%{

open Codegen

let depth = ref 0
let depth_incr f = incr depth; f !depth
let depth_reset = depth := 0

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
%type <unit> program

%%

program:
|   begin_stmt statements end_stmt EOF { raise End_of_file }
;

begin_stmt:
|   BEGIN { generate_begin () }
;

end_stmt:
|   END { generate_end () }
;

statements:
| { }
| statement SEMICOLON statements { }
;

statement:
| IDENTIFIER ASSIGN expression { generate_assign $1 $3; depth_reset }
| READ LEFTPAREN identifier_list RIGHTPAREN { generate_reads $3 }
| WRITE LEFTPAREN expression_list RIGHTPAREN { generate_writes $3; depth_reset }
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
| IDENTIFIER { var $1 }
| LITERAL { generate_literal $1 } 
| addop { $1 }
| subop { $1 }
;

addop:
| LITERAL ADDOP LITERAL { generate_literal ($1 + $3) }
| expression ADDOP expression { (depth_incr generate_add) $1 $3 }
;

subop:
| LITERAL SUBOP LITERAL { generate_literal ($1 - $3) }
| expression SUBOP expression { (depth_incr generate_sub) $1 $3 }
;

%%
