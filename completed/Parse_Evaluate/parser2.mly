%{
open Printf
type tree = Empty | Node of (string*tree*tree)
let x =  Hashtbl.create 50;;
let x1 =  Hashtbl.create 50;;
let rec printerr (t:tree) = match t with
|Empty-> ""
|Node(x,Empty,Empty)->x
|Node(x,Empty,y)->x^"("^(printerr y)^")"
|Node(x,y,Empty)->x^"("^(printerr y)^")"
|Node(x,y,y2)->x^"("^(printerr y)^","^(printerr y2)^")"
%}
/* Ocamlyacc Declarations */
%token <int> INTEGER 
%token <string> VARIABLE
%token <bool> Bool 
%token OPEN CLOSE ABS  MOD NOT AND OR EQUAL GREATER_THAN LESS_THAN GREATER_THAN_OR_EQUAL LESS_OR_EQUAL AEQUAL
%token PLUS MINUS  MULTIPLY DIVIDE DELIMITER EOL EOF UNARY


%start input
%type <unit> input
%%

input:  { }
| input line { }
;

line: EOL { }
| VARIABLE AEQUAL INTEGER EOL { Hashtbl.add x $1 (Node("Variable"^"("^$1^"("^string_of_int($3)^")"^")",Empty,Empty)) ;flush stdout }
| VARIABLE AEQUAL Bool EOL { Hashtbl.add x1 $1 (Node("Variable"^"("^$1^"("^string_of_bool($3)^")"^")",Empty,Empty)) ;flush stdout }
| exp EOL {  printf "%s\n" (printerr($1));flush stdout }
| exp_bool EOL {  printf "%s\n" (printerr($1));flush stdout }
| comp EOL {  flush stdout }
;

exp_bool:
| exp_bool AND exp_bool   { Node("AND",$1, $3) }
| exp_bool OR  exp_bool   { Node("OR",$1, $3) }
|opBool   { $1 }
;
opBool:
| NOT opBool { Node("NOT",Empty, $2) }
| Bool {Node(string_of_bool($1),Empty,Empty)}
| OPEN opBool CLOSE { $2 }
| VARIABLE { 
    try
	Hashtbl.find x1 $1
    with
      Not_found -> 
let xe = $1 in
printf "Enter Variable  %s \n" xe; 
let d = read_line() in
if(d = "T" ) then 
(Hashtbl.add x1 $1 (Node("Variable"^"("^$1^"("^d^")"^")",Empty,Empty));
Node("Variable"^"("^$1^"("^d^")"^")",Empty,Empty);)
else if(d = "F" ) then 
(Hashtbl.add x1 $1 (Node("Variable"^"("^$1^"("^d^")"^")",Empty,Empty));
Node("Variable"^"("^$1^"("^d^")"^")",Empty,Empty);)
else
(
printf "Please Enter T or F for true or false .Assuming true and calculating. \n"; 
Hashtbl.add x1 $1 (Node("Variable"^"("^$1^"("^"T"^")"^")",Empty,Empty));
Node("Variable"^"("^$1^"("^"T"^")"^")",Empty,Empty);) 
}
| comp { $1 }
;

comp:
| exp GREATER_THAN exp  {Node("Greater_than",$1, $3)}
| exp GREATER_THAN_OR_EQUAL exp {Node("AND",$1, $3)}
| exp EQUAL exp  {Node("EQUAL",$1, $3)}
| exp LESS_THAN exp {Node("LESS_THAN",$1, $3)}
| exp LESS_OR_EQUAL exp  {Node("LESS_OR_EQUAL",$1, $3)}
;
exp: 
| exp PLUS  term {Node("PLUS",$1, $3)}
| exp MINUS  term {Node("MINUS",$1, $3)}
| term             {$1}
;

term:
| term  MULTIPLY factor {Node("MULTIPLY",$1, $3)}
| term DIVIDE  factor {Node("DIVIDE",$1, $3)}
| term MOD factor {Node("MOD",$1, $3)}
| factor   { $1 }
;

factor:
| INTEGER {Node(string_of_int($1),Empty, Empty)}
| UNARY exp {Node("UNARY",$2, Empty)}
| NOT exp  {Node("NOT",$2, Empty)}
| OPEN exp CLOSE { $2 }
| ABS  exp { Node("ABS",$2, Empty) }
| VARIABLE { 
    try
	Hashtbl.find x $1
    with
      Not_found -> 
let xe = $1 in
printf "Enter Variable  %s \n" xe; 
let d = int_of_string(read_line()) in
Hashtbl.add x $1 (Node("Variable"^"("^$1^"("^string_of_int(d)^")"^")",Empty,Empty)) ;
Node("Variable"^"("^$1^"("^string_of_int(d)^")"^")",Empty,Empty);
}
;
%%
