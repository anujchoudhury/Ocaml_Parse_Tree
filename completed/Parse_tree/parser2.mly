%{
open Printf
type tree = Empty | Node of (string*tree*tree)
let x =  Hashtbl.create 50;;
let x1 =  Hashtbl.create 50;;
%}
/* Ocamlyacc Declarations */
%token <int> INTEGER 
%token <string> VARIABLE
%token <bool> Bool 
%token OPEN CLOSE ABS EXPONENTIAL MOD TRUE FALSE NOT AND OR EQUAL GREATER_THAN LESS_THAN GREATER_THAN_OR_EQUAL LESS_OR_EQUAL AEQUAL
%token PLUS MINUS  MULTIPLY DIVIDE  IF THEN ELSE IDENTIFIER DELIMITER DEF EOL EOF UNARY


%start input
%type <unit> input
/* Grammar follows */
%%

input:  { }
| input line { }
;

line: EOL { }
| exp EOL { printf "%d\n" $1; Hashtbl.clear x;flush stdout }
| exp_bool EOL { printf "%b\n" $1; Hashtbl.clear x1;flush stdout }
| comp EOL { printf "%b\n" $1;Hashtbl.clear x1; flush stdout }
;

exp_bool:
| exp_bool AND exp_bool   { $1 && $3 }
| exp_bool OR  exp_bool   { $1 || $3 }
|opBool   { $1 }
;
opBool:
| NOT opBool { not($2) }
| OPEN opBool CLOSE { $2 }
| Bool {$1}
| VARIABLE { 
    try
	Hashtbl.find x1 $1
    with
      Not_found -> 
let xe = $1 in
printf "Enter Variable  %s \n" xe; 
let d = read_line() in
if(d = "T" ) then 
(Hashtbl.add x1 $1 true;
true;)
else if(d = "F" ) then 
(Hashtbl.add x1 $1 false;
false; )
else
(
printf "Please Enter T or F for true or false .Assuming true and calculating. \n"; 
Hashtbl.add x1 $1 true;
true; )
}
| comp {$1}
;
comp:
| exp GREATER_THAN term  {if ($1 > $3) then true else false}
| exp GREATER_THAN_OR_EQUAL term {if ($1 >=$3) then true else false}
| exp EQUAL term  {if ($1 = $3) then true else false}
| exp LESS_THAN term {if ($1 < $3) then true else false}
| exp LESS_OR_EQUAL term  {if ($1 <= $3) then true else false}
;
exp: 
| exp PLUS  term { $1 + $3 }
| exp MINUS  term { $1 - $3 }
| term             { $1 }
;

term:
| term  MULTIPLY factor { $1 * $3 }
| term DIVIDE  factor { $1 / $3 }
| term MOD factor {$1 mod $3}
| factor   { $1 }
;

factor:
| INTEGER { $1 }
| UNARY exp { -1 * $2 }
| NOT exp  { if $2 = 1 then 1 else 0 }
| OPEN exp CLOSE { $2 }
| ABS  exp { abs $2 }
| VARIABLE { 
    try
	Hashtbl.find x $1
    with
      Not_found -> 
let xe = $1 in
printf "Enter Variable  %s \n" xe; 
let d = int_of_string(read_line()) in
Hashtbl.add x $1 d;
d; 
}
;
%%


