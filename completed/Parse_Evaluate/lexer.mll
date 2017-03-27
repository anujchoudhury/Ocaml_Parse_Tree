{
open Parser	
}

rule token = parse
[' ' '\t']+ { token lexbuf }
| "<-" {AEQUAL}
| "abs"     {ABS}
| '\n'      { EOL }
| '+' { PLUS }
| '-' { MINUS }
| '*' { MULTIPLY }
| "div"  { DIVIDE }
| "mod"  { MOD }
| '(' { OPEN }
| '0'|['1'-'9']['0'-'9']* as lxm{  INTEGER(int_of_string lxm)  }	
| ')' { CLOSE }
| "T" { Bool(true) }
| "F" { Bool(false) }
| "/\\" { AND }
| "\\/" { OR }
| '=' { EQUAL }
| "~" { UNARY}
| "not" { NOT }
| '>' { GREATER_THAN }
| '<' { LESS_THAN }
| ">="  { GREATER_THAN_OR_EQUAL }
| "<="  { LESS_OR_EQUAL }
| ['A'-'Z']['0'-'9''a'-'z''A'-'Z''_''\'']* as lxm{ VARIABLE(lxm) }
| eof { EOF }
|_    { print_string("Invalid_input "); token lexbuf}
	
