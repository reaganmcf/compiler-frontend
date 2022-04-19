/**********************************************
        CS415  Project 2
        Spring  2022
        Student Version
**********************************************/

#ifndef ATTR_H
#define ATTR_H

typedef union {
  int num;
  char *str;
} tokentype;

typedef enum simple_types {
  TYPE_INT = 0,
  TYPE_BOOL,
  TYPE_ERROR,
  TYPE_INT_ARRAY,
  TYPE_BOOL_ARRAY 
} Simple_Type;

typedef union type_data {
  int array_size; 
} typeData;

typedef struct type_expression {
  Simple_Type type;
  typeData data;
} Type_Expression;

typedef struct {
  Type_Expression typeExpr;
  int targetRegister;
} regInfo;

typedef struct {
  char** ids;
  int count;
} idlistInfo;

void add_id_to_idlist(idlistInfo* idlist, char* id);

typedef struct {
  char* id;
} ctrlexpInfo;

typedef struct {
  int cmp_label;
  int break_label;
} forLoopInfo;

typedef struct {
  int true_label;
  int false_label;
  int after_else_label;
} ifHeadInfo;

typedef struct {
  int cmp_label;
  int body_label;
  int break_label;
} whileLoopInfo;

#endif
