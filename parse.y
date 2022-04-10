%{
#include <stdio.h>
#include <stdlib.h>
#include "attr.h"
#include "instrutil.h"
int yylex();
void yyerror(char * s);
#include "symtab.h"

FILE *outfile;
char *CommentBuffer;
 
%}

%union {
  tokentype token;
  regInfo targetReg;
  idlistInfo idlist;
  Type_Expression typeExpr;
}

%token PROG PERIOD VAR 
%token INT BOOL PRT THEN IF DO FI ENDWHILE ENDFOR
%token ARRAY OF 
%token BEG END ASG  
%token EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE
%token WHILE FOR ELSE 
%token <token> ID ICONST 

%type <targetReg> exp 
%type <targetReg> lhs 

%type <idlist> idlist

%type <typeExpr> stype
%type <typeExpr> type

%start program

%nonassoc EQ NEQ LT LEQ GT GEQ 
%left '+' '-' AND
%left '*' OR

%nonassoc THEN
%nonassoc ELSE

%%
program : {
            emitComment("Assign STATIC_AREA_ADDRESS to register \"r0\"");
            emit(NOLABEL, LOADI, STATIC_AREA_ADDRESS, 0, EMPTY);
          } 
           PROG ID ';' block PERIOD { }
	;

block	: variables cmpdstmt { }
	;

variables: /* empty */
	| VAR vardcls { }
	;

vardcls	: vardcls vardcl ';' { }
	| vardcl ';' {}
	| error ';' { yyerror("***Error: illegal variable declaration\n");}  
	;

vardcl	: idlist ':' type {
                            //sprintf(CommentBuffer, "idlist has the following members:");
                            //emitComment(CommentBuffer);
                            //for(int i = 0; i < $1.count; i++) {
                            //  sprintf(CommentBuffer, "\t%s", $1.ids[i]);
                            //  emitComment(CommentBuffer);
                            //}

                            // For each id - add an entry in the lookup table
                            for (int i = 0; i < $1.count; i++) {
                              int offset = NextOffset(1);
                              char* curr_id = $1.ids[i];
                              
                              SymTabEntry *entry = lookup(curr_id);
                              if (entry != NULL) {
                                printf("\n***Error: duplicate declaration of %s\n", curr_id);
                              } else {
                                insert(curr_id, $3, offset);
                              }
                            }
                          }
	;

idlist	: idlist ',' ID {
                          $$ = $1;
                          add_id_to_idlist(&$$, $3.str);
                        }
  | ID		{ 
            $$.ids = calloc(100, sizeof(char*));
            $$.count = 0;

            add_id_to_idlist(&$$, $1.str);
          } 
	;


type	: ARRAY '[' ICONST ']' OF stype {  }

        | stype {  }
	;

stype	: INT { 
              $$ = TYPE_INT
            }
  | BOOL {
            $$ = TYPE_BOOL
         }
	;

stmtlist : stmtlist ';' stmt { }
	| stmt { }
        | error { yyerror("***Error: ';' expected or illegal statement \n");}
	;

stmt    : ifstmt { }
	| fstmt { }
	| wstmt { }
	| astmt { }
	| writestmt { }
	| cmpdstmt { }
	;

cmpdstmt: BEG stmtlist END { }
	;

ifstmt :  ifhead 
          THEN
          stmt 
  	  ELSE 
          stmt 
          FI
	;

ifhead : IF condexp {  }
        ;

writestmt: PRT '(' exp ')' { 
                              int printOffset = -4; /* default location for printing */
  	                          sprintf(CommentBuffer, "Code for \"PRINT\" from offset %d", printOffset);
                              emitComment(CommentBuffer);
                              emit(NOLABEL, STOREAI, $3.targetRegister, 0, printOffset);
                              emit(
                                NOLABEL, 
                                OUTPUTAI, 
                                0,
                                printOffset, 
                                EMPTY
                              );
                           }
	;

fstmt	: FOR ctrlexp DO stmt { }
          ENDFOR
	;

wstmt	: WHILE condexp DO stmt { }
          ENDWHILE
        ;
  

astmt : lhs ASG exp  { 
                        //sprintf(CommentBuffer, "$1.type = %d, $3.type = %d", $1.type, $3.type);
                        //emitComment(CommentBuffer);
                        int both_ints = $1.type == TYPE_INT && $3.type == TYPE_INT;
                        int both_bools = $1.type == TYPE_BOOL && $3.type == TYPE_BOOL;
                        if (!both_ints || !both_bools) {
                          printf("*** ERROR ***: Assignment types do not match.\n");
                        }

                        emit(
                          NOLABEL,
                          STORE, 
                          $3.targetRegister,
                          $1.targetRegister,
                          EMPTY
                        );
                     }
	;

lhs	: ID	{           
            //sprintf(CommentBuffer, "looking up %s", $1.str);
            //emitComment(CommentBuffer);
            SymTabEntry *entry = lookup($1.str);
            if (entry == NULL) {
              printf("\n***Error: undeclared identifier %s\n", $1.str);
            } else {
              int newReg1 = NextRegister();
              int newReg2 = NextRegister();

              $$.targetRegister = newReg2;
              $$.type = entry->type;
               
              sprintf(
                CommentBuffer, 
                "Compute address of variable \"%s\" at offset %d in register %d",
                entry->name,
                entry->offset,
                newReg2
              );
              emitComment(CommentBuffer);
                
              emit(NOLABEL, LOADI, entry->offset, newReg1, EMPTY);
              emit(NOLABEL, ADD, 0, newReg1, newReg2);
            }
          }


  |  ID '[' exp ']' {   }
  ;


exp	: exp '+' exp		{ 
                      int newReg = NextRegister();

                      if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
                        printf("*** ERROR ***: Operator types must be integer.\n");
                      }
                      $$.type = $1.type;

                      $$.targetRegister = newReg;
                      emit(NOLABEL, 
                           ADD, 
                           $1.targetRegister, 
                           $3.targetRegister, 
                           newReg
                      );
                    }

  | exp '-' exp		{  }

  | exp '*' exp		{  }

  | exp AND exp		{  } 


  | exp OR exp    {  }


  | ID	{ 
          SymTabEntry* entry = lookup($1.str);
          if (entry == NULL) {
            printf("\n***Error: undeclared identifier %s\n", $1.str);
          } else {
            sprintf(
              CommentBuffer, 
              "Load RHS value of variable \"%s\" at offset %d",
              entry->name,
              entry->offset
            );
            emitComment(CommentBuffer);
            int newReg = NextRegister();
            int offset = entry->offset;

            $$.targetRegister = newReg;
            $$.type = entry->type;
            emit(NOLABEL, LOADAI, 0, offset, newReg);
          }
        }

  | ID '[' exp ']'	{   }
 


	| ICONST  { 
              int newReg = NextRegister();
              $$.targetRegister = newReg;
              $$.type = TYPE_INT;
              emit(NOLABEL, LOADI, $1.num, newReg, EMPTY); 
            }

  | TRUE  { 
            int newReg = NextRegister(); /* TRUE is encoded as value '1' */
            $$.targetRegister = newReg;
            $$.type = TYPE_BOOL;
            emit(NOLABEL, LOADI, 1, newReg, EMPTY); 
          }

  | FALSE {
            int newReg = NextRegister(); /* TRUE is encoded as value '0' */
            $$.targetRegister = newReg;
            $$.type = TYPE_BOOL;
            emit(NOLABEL, LOADI, 0, newReg, EMPTY);
          }

	| error { yyerror("***Error: illegal expression\n");}  
	;


ctrlexp	: ID ASG ICONST ',' ICONST { }
        ;

condexp	: exp NEQ exp		{  } 

  | exp EQ exp		{  } 

  | exp LT exp		{  }

  | exp LEQ exp		{  }

	| exp GT exp		{  }

	| exp GEQ exp		{  }

	| error { yyerror("***Error: illegal conditional expression\n");}  
  ;

%%

void yyerror(char* s) {
  fprintf(stderr,"%s\n",s);
}


int main(int argc, char* argv[]) {

  printf("\n     CS415 Spring 2022 Compiler\n\n");

  outfile = fopen("iloc.out", "w");
  if (outfile == NULL) { 
    printf("ERROR: Cannot open output file \"iloc.out\".\n");
    return -1;
  }

  CommentBuffer = (char *) malloc(1961);  
  InitSymbolTable();

  printf("1\t");
  yyparse();
  printf("\n");

  PrintSymbolTable();
  
  fclose(outfile);
  
  return 1;
}




