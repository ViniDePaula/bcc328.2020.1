// parser.mly

%token                 EOF
%token <int>           LITINT
%token <Symbol.symbol> ID
%token                 PLUS
%token                 LT
%token                 EQ
%token                 COMMA
%token                 LPAREN
%token                 RPAREN
%token                 INT
%token                 BOOL
%token                 IF
%token                 THEN
%token                 ELSE
%token                 LET
%token                 IN

%start <Absyn.lexp> program

%left PLUS

%%

program:
| x=exp EOF        { x }

exp:
| x=LITINT         { ($loc , Absyn.IntExp x) }
| x=exp PLUS y=exp { ($loc , Absyn.OpExp (Absyn.Plus, x, y)) }
