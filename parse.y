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
char *ErrorBuffer;
 
%}

%union {
  tokentype token;
  regInfo targetReg;
  idlistInfo idlist;
  Type_Expression typeExpr;
  ctrlexpInfo ctrlexpInfo;
  forLoopInfo forInfo;
  ifHeadInfo ifInfo;
  whileLoopInfo whileInfo;
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
%type <targetReg> condexp

%type <idlist> idlist

%type <typeExpr> stype
%type <typeExpr> type

%type <ctrlexpInfo> ctrlexp; 
%type <forInfo> FOR;
%type <whileInfo> WHILE;
%type <ifInfo> ifhead;

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
                            // For each id - add an entry in the lookup table
                            int offset_size = 1;
                            if ($3.type == TYPE_INT_ARRAY || $3.type == TYPE_BOOL_ARRAY) {
                              offset_size = $3.data.array_size;
                            }
                            for (int i = 0; i < $1.count; i++) {
                              int offset = NextOffset(offset_size);
                              char* curr_id = $1.ids[i];
                              
                              SymTabEntry *entry = lookup(curr_id);
                              if (entry != NULL) {
                                sprintf(ErrorBuffer, "\n***Error: duplicate declaration of %s\n", curr_id);
                                yyerror(ErrorBuffer);
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


type	: ARRAY '[' ICONST ']' OF stype {
                                        if ($6.type == TYPE_INT) {
                                          $$.type = TYPE_INT_ARRAY;
                                          $$.data.array_size = $3.num;
                                        } else if ($6.type == TYPE_BOOL) {
                                          $$.type = TYPE_BOOL_ARRAY;
                                          $$.data.array_size = $3.num;
                                        } else {
                                          yyerror("**Error: arrays can only be simple types\n");
                                        }
                                      }

        | stype { 
                  $$ = $1;
                }
	;

stype	: INT { 
              $$.type = TYPE_INT
            }
  | BOOL {
            $$.type = TYPE_BOOL
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
          THEN {
                emit($1.true_label, NOP, EMPTY, EMPTY, EMPTY);
                sprintf(CommentBuffer, "This is the \"true\" branch");
                emitComment(CommentBuffer);
               }
          stmt 
              {
                sprintf(CommentBuffer, "Branch to statement following the \"else\" statement list");
                emitComment(CommentBuffer);
                emit(NOLABEL, BR, $1.after_else_label, EMPTY, EMPTY);
              }
  	  ELSE 
               {
                emit($1.false_label, NOP, EMPTY, EMPTY, EMPTY);
                sprintf(CommentBuffer, "This is the \"false\" branch");
                emitComment(CommentBuffer);
               }
          stmt 
          FI
            {
              emit($1.after_else_label, NOP, EMPTY, EMPTY, EMPTY);
            }
	;

ifhead : IF condexp {
                      if ($2.typeExpr.type != TYPE_BOOL) {
                        yyerror("\n***Error: exp in if stmt must be boolean\n");
                      } else {
                        int true_label = NextLabel();
                        int false_label = NextLabel();
                        int after_else_label = NextLabel();

                        emit(NOLABEL, CBR, $2.targetRegister, true_label, false_label);
                        
                        $$.true_label = true_label;
                        $$.false_label = false_label;
                        $$.after_else_label = after_else_label;
                      }
                    }
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

fstmt	: FOR ctrlexp {
                      sprintf(CommentBuffer, "Generate control code for \"FOR\" ");
                      emitComment(CommentBuffer);
                      
                      SymTabEntry* id_entry = lookup($2.id);
                      if (id_entry == NULL) {
                        printf("ICE: id_entry is NULL\n");
                        exit(1);
                      }

                      int cmp_branch = NextLabel();
                      int body_branch = NextLabel();
                      int break_branch = NextLabel();
        
                      $1.cmp_label = cmp_branch;
                      $1.break_label = break_branch;

                      int reg1 = NextRegister();
                      int reg2 = NextRegister();
                      // upper bound reg hasn't been created yet, but it's always 3 more than this one
                      int ub_reg = reg1 + 3; 
                      emit(cmp_branch, LOADAI, 0, id_entry->offset, reg1);
                      emit(NOLABEL, CMPLE, reg1, ub_reg, reg2);
                      emit(NOLABEL, CBR, reg2, body_branch, break_branch);

                      emit(body_branch, NOP, EMPTY, EMPTY, EMPTY);

                      // Lb and Ub registers are hardcoded from before,
                      // so we need to skip over them
                      NextRegister();
                      NextRegister();
                    }
          DO stmt { 
                    SymTabEntry* id_entry = lookup($2.id);
                    if (id_entry == NULL) {
                      printf("ICE: id_entry is NULL\n");
                      exit(1);
                    }

                    int reg1 = NextRegister();
                    int reg2 = NextRegister();
                    emit(NOLABEL, LOADAI, 0, id_entry->offset, reg1);
                    emit(NOLABEL, ADDI, reg1, 1, reg2);
                    emit(NOLABEL, STOREAI, reg2, 0, id_entry->offset);
                    emit(NOLABEL, BR, $1.cmp_label, EMPTY, EMPTY);
                    emit($1.break_label, NOP, EMPTY, EMPTY, EMPTY);
                  }
          ENDFOR
	;

wstmt	: WHILE { 
                int cmp_label = NextLabel();
                emit(cmp_label, NOP, EMPTY, EMPTY, EMPTY);

                $1.cmp_label = cmp_label;

                sprintf(CommentBuffer, "Control code for \"WHILE DO\"");
                emitComment(CommentBuffer);
              }
      condexp {
                if ($3.typeExpr.type != TYPE_BOOL) {
                  yyerror("\n***Error: exp in while stmt must be boolean\n");
                } else {
                  int body_label = NextLabel();
                  int break_label = NextLabel();

                  $1.body_label = body_label;
                  $1.break_label = break_label;

                  emit(NOLABEL, CBR, $3.targetRegister, body_label, break_label);

                  emit(body_label, NOP, EMPTY, EMPTY, EMPTY);
                }
              }
           DO {
                sprintf(CommentBuffer, "Body of \"WHILE\" construct starts here");
                emitComment(CommentBuffer);
              } 
         stmt {
                emit(NOLABEL, BR, $1.cmp_label, EMPTY, EMPTY);
                emit($1.break_label, NOP, EMPTY, EMPTY, EMPTY);
              }
      ENDWHILE
        ;
  

astmt : lhs ASG exp  { 
                        int both_ints = $1.typeExpr.type == TYPE_INT && $3.typeExpr.type == TYPE_INT;
                        int both_bools = $1.typeExpr.type == TYPE_BOOL && $3.typeExpr.type == TYPE_BOOL;
                        if (!(both_ints || both_bools)) {
                          sprintf(ErrorBuffer, "\n***Error: assignment types do not match\n");
                          yyerror(ErrorBuffer);
                        } else if ($1.typeExpr.type == TYPE_INT_ARRAY || $1.typeExpr.type == TYPE_BOOL_ARRAY) {
                          sprintf(ErrorBuffer, "\n***Error: assignment to whole array\n");
                          yyerror(ErrorBuffer);
                        } else {
                          emit(
                            NOLABEL,
                            STORE, 
                            $3.targetRegister,
                            $1.targetRegister,
                            EMPTY
                          );
                        }
                     }
	;

lhs	: ID	{           
            SymTabEntry *entry = lookup($1.str);
            if (entry == NULL) {
              sprintf(ErrorBuffer, "\n***Error: undeclared identifier %s\n", $1.str);
              yyerror(ErrorBuffer);
            } else {
              int newReg1 = NextRegister();
              int newReg2 = NextRegister();

              $$.targetRegister = newReg2;
              $$.typeExpr = entry->typeExpr;
               
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


  |  ID '[' exp ']' {
                      SymTabEntry *entry = lookup($1.str);
                      if (entry == NULL) {
                        printf("\n***Error: undeclared identifier %s\n", $1.str);
                      } else if ($3.typeExpr.type != TYPE_INT) {
                        sprintf(ErrorBuffer, "\n***Error: subscript exp not type integer\n");
                        yyerror(ErrorBuffer);
                      } else if (entry->typeExpr.type == TYPE_INT_ARRAY || entry->typeExpr.type == TYPE_BOOL_ARRAY) {
                        sprintf(ErrorBuffer, "\n***Error: id %s is not an array\n", entry->name);
                        yyerror(ErrorBuffer);
                      } else {
                        sprintf(
                          CommentBuffer, 
                          "Compute address of array variable \"%s\" with base address %d",
                          entry->name,
                          entry->offset
                        );
                        emitComment(CommentBuffer);
                      
                        int actual_address_reg = NextRegister();
                        
                        int elem_size_reg = NextRegister();
                        emit(NOLABEL, LOADI, 4, elem_size_reg, EMPTY);

                        int offset_address_reg = NextRegister();
                        emit(NOLABEL, MULT, $3.targetRegister, elem_size_reg, offset_address_reg);

                        int base_address_reg = NextRegister();
                        emit(NOLABEL, LOADI, entry->offset, base_address_reg, EMPTY);

                        int relative_address_reg = NextRegister();
                        emit(NOLABEL, ADD, base_address_reg, offset_address_reg, relative_address_reg);

                        emit(NOLABEL, ADD, 0, relative_address_reg, actual_address_reg);

                        $$.targetRegister = actual_address_reg;
                      }
                    }
  ;


exp	: exp '+' exp		{ 
                      int newReg = NextRegister();

                      if (! (($1.typeExpr.type == TYPE_INT) && ($3.typeExpr.type == TYPE_INT))) {
                        sprintf(ErrorBuffer, "\n***Error: types of operands for operation %s do not match\n", "+"); 
                        yyerror(ErrorBuffer);
                      }
                      $$.typeExpr = $1.typeExpr;

                      $$.targetRegister = newReg;
                      emit(NOLABEL, 
                           ADD, 
                           $1.targetRegister, 
                           $3.targetRegister, 
                           newReg
                      );
                    }

  | exp '-' exp		{  
                    int newReg = NextRegister();

                    if (! (($1.typeExpr.type == TYPE_INT) && ($3.typeExpr.type == TYPE_INT))) {
                      sprintf(ErrorBuffer, "\n***Error: types of operands for operation %s do not match\n", "-"); 
                      yyerror(ErrorBuffer);
                    }
                    $$.typeExpr = $1.typeExpr;

                    $$.targetRegister = newReg;
                    emit(NOLABEL, 
                         SUB, 
                         $1.targetRegister, 
                         $3.targetRegister, 
                         newReg
                    );
                  }

  | exp '*' exp		{
                    int newReg = NextRegister();

                    if (! (($1.typeExpr.type == TYPE_INT) && ($3.typeExpr.type == TYPE_INT))) {
                      sprintf(ErrorBuffer, "\n***Error: types of operands for operation %s do not match\n", "*"); 
                      yyerror(ErrorBuffer);
                    }
                    $$.typeExpr = $1.typeExpr;

                    $$.targetRegister = newReg;
                    emit(NOLABEL, 
                         MULT, 
                         $1.targetRegister, 
                         $3.targetRegister, 
                         newReg
                    );
                  }

  | exp AND exp		{  
                    int both_bools = $1.typeExpr.type == TYPE_BOOL && $3.typeExpr.type == TYPE_BOOL;

                    if (!both_bools) {
                      sprintf(ErrorBuffer, "\n***Error: types of operands for operation %s do not match\n", "AND"); 
                      yyerror(ErrorBuffer);
                    } else { 
                      int newReg = NextRegister();
                      $$.typeExpr = $1.typeExpr;

                      $$.targetRegister = newReg;
                      emit(
                        NOLABEL, 
                        AND_INSTR, 
                        $1.targetRegister, 
                        $3.targetRegister, 
                        newReg
                      );
                    }
                  } 


  | exp OR exp    {  
                    int both_bools = $1.typeExpr.type == TYPE_BOOL && $3.typeExpr.type == TYPE_BOOL;

                    if (!both_bools) {
                      sprintf(ErrorBuffer, "\n***Error: types of operands for operation %s do not match\n", "OR"); 
                      yyerror(ErrorBuffer);
                    } else { 
                      int newReg = NextRegister();

                      $$.typeExpr = $1.typeExpr;

                      $$.targetRegister = newReg;
                      emit(
                        NOLABEL, 
                        OR_INSTR, 
                        $1.targetRegister, 
                        $3.targetRegister, 
                        newReg
                      );
                    }
                  }


  | ID	{ 
          SymTabEntry* entry = lookup($1.str);
          if (entry == NULL) {
            sprintf(ErrorBuffer, "\n***Error: undeclared identifier %s\n", $1.str);
            yyerror(ErrorBuffer);
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
            $$.typeExpr = entry->typeExpr;
            emit(NOLABEL, LOADAI, 0, offset, newReg);
          }
        }

  | ID '[' exp ']'	{
                      SymTabEntry *entry = lookup($1.str);
                      if (entry == NULL) {
                        sprintf(ErrorBuffer, "***Error: undeclared identifier %s\n", $1.str);
                        yyerror(ErrorBuffer);
                      } else if ($3.typeExpr.type != TYPE_INT) {
                        yyerror("\n***Error: subscript exp not type integer\n");
                      } else if (entry->typeExpr.type != TYPE_INT_ARRAY && entry->typeExpr.type != TYPE_BOOL_ARRAY) {
                        sprintf(ErrorBuffer, "\n***Error: id %s is not an array\n", entry->name);
                        yyerror(ErrorBuffer);
                      } else {
                        sprintf(
                          CommentBuffer, 
                          "Load RHS value of array variable \"%s\" with based address %d",
                          entry->name,
                          entry->offset
                        );
                        emitComment(CommentBuffer);

                        int actual_address_reg = NextRegister();
                        
                        int elem_size_reg = NextRegister();
                        emit(NOLABEL, LOADI, 4, elem_size_reg, EMPTY);

                        int offset_address_reg = NextRegister();
                        emit(NOLABEL, MULT, $3.targetRegister, elem_size_reg, offset_address_reg);

                        int base_address_reg = NextRegister();
                        emit(NOLABEL, LOADI, entry->offset, base_address_reg, EMPTY);

                        int relative_address_reg = NextRegister();
                        emit(NOLABEL, ADD, base_address_reg, offset_address_reg, relative_address_reg);

                        emit(NOLABEL, LOADAO, 0, relative_address_reg, actual_address_reg);
                        $$.targetRegister = actual_address_reg;
                      }
                    }
 


	| ICONST  { 
              int newReg = NextRegister();
              $$.targetRegister = newReg;
              $$.typeExpr.type = TYPE_INT;
              emit(NOLABEL, LOADI, $1.num, newReg, EMPTY); 
            }

  | TRUE  { 
            int newReg = NextRegister(); /* TRUE is encoded as value '1' */
            $$.targetRegister = newReg;
            $$.typeExpr.type = TYPE_BOOL;
            emit(NOLABEL, LOADI, 1, newReg, EMPTY); 
          }

  | FALSE {
            int newReg = NextRegister(); /* FALSE is encoded as value '0' */
            $$.targetRegister = newReg;
            $$.typeExpr.type = TYPE_BOOL;
            emit(NOLABEL, LOADI, 0, newReg, EMPTY);
          }

	| error { yyerror("***Error: illegal expression\n");}  
	;


ctrlexp	: ID ASG ICONST ',' ICONST  
    { 
      SymTabEntry* entry = lookup($1.str);
      if (entry == NULL) {
        sprintf(ErrorBuffer, "\n***Error: undeclared identifier %s\n", $1.str);
        yyerror(ErrorBuffer);
      } else if ($3.num > $5.num) {
        yyerror("\n***Error: lower bound exceeds upper bound\n");
      } else if (entry->typeExpr.type != TYPE_INT) {
        yyerror("\n***Error: induction variable not scalar integer variable\n");
      } else {
        int newReg1 = NextRegister(); 
        int newReg2 = NextRegister(); 
        // the order is so weird on the solution compiler,
        // so we hard code these and incremenet the NextRegister
        // manually later
        int lbReg = newReg2 + 3;
        int ubReg = lbReg + 1;

        int lb = $3.num;
        int ub = $5.num;

        $$.id = $1.str;

        sprintf(
          CommentBuffer, 
          "Initialize ind. variable \"%s\" at offset %d with lower bound value %d",
          entry->name,
          entry->offset,
          lb
        );
        emitComment(CommentBuffer);
        emit(NOLABEL, LOADI, entry->offset, newReg1, EMPTY);
        emit(NOLABEL, ADD, 0, newReg1, newReg2);
        emit(NOLABEL, LOADI, lb, lbReg, EMPTY);
        emit(NOLABEL, LOADI, ub, ubReg, EMPTY);
        emit(NOLABEL, STORE, lbReg, newReg2, EMPTY);
      }
    }
        ;

condexp	: exp NEQ exp		{
                          int both_ints = $1.typeExpr.type == TYPE_INT && $3.typeExpr.type == TYPE_INT;
                          int both_bools = $1.typeExpr.type == TYPE_BOOL && $3.typeExpr.type == TYPE_BOOL;
                          if (!both_bools && !both_ints) {
                            sprintf(ErrorBuffer, "\n***Error: types of operands for operation %s do not match\n", "!=");
                            yyerror(ErrorBuffer);
                          } else {
                            int reg1 = NextRegister();
                            
                            emit(NOLABEL, CMPNE, $1.targetRegister, $3.targetRegister, reg1);

                            $$.typeExpr.type = TYPE_BOOL;
                            $$.targetRegister = reg1;
                          }
                        } 

  | exp EQ exp		{
                    int both_ints = $1.typeExpr.type == TYPE_INT && $3.typeExpr.type == TYPE_INT;
                    int both_bools = $1.typeExpr.type == TYPE_BOOL && $3.typeExpr.type == TYPE_BOOL;
                    if (!both_bools && !both_ints) {
                      sprintf(ErrorBuffer, "\n***Error: types of operands for operation %s do not match\n", "==");
                      yyerror(ErrorBuffer);
                    } else {
                      int reg1 = NextRegister();
                      emit(NOLABEL, CMPEQ, $1.targetRegister, $3.targetRegister, reg1);

                      $$.typeExpr.type = TYPE_BOOL;
                      $$.targetRegister = reg1;
                    }
                  }

  | exp LT exp		{  
                    int both_ints = $1.typeExpr.type == TYPE_INT && $3.typeExpr.type == TYPE_INT;
                    if (!both_ints) {
                      sprintf(ErrorBuffer, "\n***Error: types of operands for operation %s do not match\n", "<");
                      yyerror(ErrorBuffer);
                    } else {
                      int reg1 = NextRegister();
                      emit(NOLABEL, CMPLT, $1.targetRegister, $3.targetRegister, reg1);

                      $$.typeExpr.type = TYPE_BOOL;
                      $$.targetRegister = reg1;
                    }
                  }

  | exp LEQ exp		{
                    int both_ints = $1.typeExpr.type == TYPE_INT && $3.typeExpr.type == TYPE_INT;
                    if (!both_ints) {
                      sprintf(ErrorBuffer, "\n***Error: types of operands for operation %s do not match\n", "<=");
                      yyerror(ErrorBuffer);
                    } else {
                      int reg1 = NextRegister();
                      emit(NOLABEL, CMPLE, $1.targetRegister, $3.targetRegister, reg1);

                      $$.typeExpr.type = TYPE_BOOL;
                      $$.targetRegister = reg1;
                    }
                  }

	| exp GT exp		{ 
                    int both_ints = $1.typeExpr.type == TYPE_INT && $3.typeExpr.type == TYPE_INT;
                    if (!both_ints) {
                      sprintf(ErrorBuffer, "\n***Error: types of operands for operation %s do not match\n", ">");
                      yyerror(ErrorBuffer);
                    } else {
                      int reg1 = NextRegister();
                      emit(NOLABEL, CMPGT, $1.targetRegister, $3.targetRegister, reg1);

                      $$.typeExpr.type = TYPE_BOOL;
                      $$.targetRegister = reg1;
                    }
                  }

	| exp GEQ exp		{ 
                    int both_ints = $1.typeExpr.type == TYPE_INT && $3.typeExpr.type == TYPE_INT;
                    if (!both_ints) {
                      sprintf(ErrorBuffer, "\n***Error: types of operands for operation %s do not match\n", ">=");
                      yyerror(ErrorBuffer);
                    } else {
                      int reg1 = NextRegister();
                      emit(NOLABEL, CMPGE, $1.targetRegister, $3.targetRegister, reg1);

                      $$.typeExpr.type = TYPE_BOOL;
                      $$.targetRegister = reg1;
                    }
                  }

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
  ErrorBuffer = (char *) malloc(1000);
  
  InitSymbolTable();

  printf("1\t");
  yyparse();
  printf("\n");

  PrintSymbolTable();
  
  fclose(outfile);
  
  return 1;
}




