%{ open Ast %}

%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token TIMES DIVIDE PLUS MINUS


%token GEQ GT LEQ LT EQ NEQ 

%token AND OR NOT
%token IF ELSE
%token FOR WHILE PRINT

%token ASSIGN COLON SEMI COMMA DOT
%token INT BOOL STRING FLOAT VOID NULL TRUE FALSE
// %token NODE
%token STRUCT
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT
%token <string> ID STRINGLIT
%token <string> STRUCT_ID
%token MAIN
%token RETURN
%token EOF

%left SEMI
%right ASSIGN
%left COLON
%left AND OR
%left EQ NEQ
%left GEQ GT LEQ LT
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%left LBRACKET RBRACKET
%left LPAREN RPAREN

%start program
%type <Ast.program> program

%%


program:
    { [], [] }
  | program struct_decl { ($2 :: fst $1), snd $1 }
  | program fdecl       { fst $1, ($2 :: snd $1) }

struct_decl:
  STRUCT STRUCT_ID LBRACE field_list RBRACE SEMI
  { { sname = $2;
      fields = List.rev $4;} }

fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
  { { typ = $1;
    fname = $2;
    formals = $4;
    fstmts = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                    { [($1, $2)]     }
  | formal_list COMMA typ ID   { ($3,$4) :: $1 }

field_list:
    /* nothing */ { [] }
  | field_list field   { $2 :: $1 }

field:
  typ ID SEMI {($1, $2)}

vname:
  ID {Id($1)}

stmt_list:
    { [] }
  | stmt_list stmt {$2 :: $1}

stmt:
    expr SEMI                                  { Expr($1) }
  | LBRACE stmt_list RBRACE                    { Block(List.rev $2)}
  | IF LPAREN expr RPAREN stmt ELSE stmt       { If($3, $5, $7) }
  | FOR LPAREN stmt expr SEMI expr RPAREN stmt { For($3, $4, $6, $8) }
  | WHILE LPAREN expr RPAREN stmt              { While($3, $5) }
  | typ ID ASSIGN expr SEMI                    { VarDecl($1, $2, $4) }
  | typ ID SEMI                                { VarInitial($1, $2) }
  | RETURN expr SEMI                           { Return($2) }


expr:
    LPAREN expr RPAREN            { $2 }
  | STRINGLIT                     { StringLit($1) }
  | FLOATLIT                      { FloatLit($1) }
  | INTLIT                        { IntLit($1)  }
  | BOOLLIT                       { BoolLit($1)  }
  | ID                            { Id($1) }
  | LBRACKET args_opt RBRACKET    { ArrayLit($2) }
  | ID LBRACKET expr RBRACKET ASSIGN expr { ArrayAssignOp(Id($1), $3, $6) }
  | ID LBRACKET expr RBRACKET     { ArrayIndex(Id($1), $3) }
  /*| REVERSE LPAREN expr RPAREN    { ArrayReverseOp($3) }
  | LENGTH LPAREN expr RPAREN     { ArraySizeOp($3) }*/
  | expr PLUS expr                { Binop($1, Add, $3) }
  | expr MINUS expr               { Binop($1, Sub, $3) }
  | expr TIMES expr               { Binop($1, Mul, $3) }
  | expr DIVIDE expr              { Binop($1, Div, $3) }
  | expr EQ expr                  { Binop($1, Eq, $3) }
  | expr NEQ expr                 { Binop($1, Neq, $3) }
  | expr GEQ expr                 { Binop($1, Geq, $3) }
  | expr GT expr                  { Binop($1, Gt, $3) }
  | expr LEQ expr                 { Binop($1, Leq, $3) }
  | expr LT expr                  { Binop($1, Lt, $3) }
  | expr AND expr                 { Binop($1, And, $3) }
  | expr OR expr                  { Binop($1, Or, $3) }
  | NOT expr                      { Not($2) }
  | ID LPAREN args_opt RPAREN     { Call($1, $3)  }
  | vname ASSIGN expr             { AssignOp($1, $3) }
  | ID DOT ID                     {StructAccess(Id($1), Id($3))}
  | ID DOT ID ASSIGN expr         {StructAssign(Id($1), Id($3), $5)}

args_opt:
    /* nothing */ { [] }
  | args_list     { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr    { $3 :: $1 }

typ:
  | INT                    { Int }
  | FLOAT                  { Float }
  | BOOL                   { Bool }
  | STRING                 { String }
  | VOID                   { Void }
  | STRUCT_ID {Struct($1)}
  | typ LBRACKET RBRACKET  { Array($1)}
