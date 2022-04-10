/**********************************************
        CS415  Project 2
        Spring  2022
        Student Version
**********************************************/

#include "attr.h"
#include "stdlib.h"
#include "stdio.h"
#include <stdlib.h>

void add_id_to_idlist(idlistInfo* idlist, char* id) {
  if (idlist == NULL) {
    printf("ICE: idlist is NULL!");
    exit(1);
  }

  if (idlist->ids == NULL) {
    printf("ICE: idlist ids is NULL!");
    exit(1);
  }

  if (id == NULL) {
    printf("ICE: id is NULL");
    exit(1);
  }

  idlist->ids[idlist->count] = calloc(512, sizeof(char));
  sprintf(idlist->ids[idlist->count], "%s", id);
  idlist->count = idlist->count + 1;
}

