#include "SmartCalc.h"

Node_Stack* polish_notation(Node_Stack* line) {
  line = inverse_stack(line);
  Node_Stack* oper_func = NULL;
  Node_Stack* polish = NULL;
  while (line) {
    if (operator_stack(line->type) || func(line->type)) {
      if (oper_func == NULL) {
        push(&oper_func, line->num, line->priority, line->type);
      } else if (oper_func->priority < line->priority ||
                 oper_func->type == Lbracket_1) {
        push(&oper_func, line->num, line->priority, line->type);
      } else if (oper_func->priority == line->priority) {
        push(&polish, oper_func->num, oper_func->priority, oper_func->type);
        pop(&oper_func);
        push(&oper_func, line->num, line->priority, line->type);
      } else {
        push(&polish, oper_func->num, oper_func->priority, oper_func->type);
        pop(&oper_func);
        if (polish->priority >= 2) {
          if (oper_func != NULL) {
            if (oper_func->priority >= 1 && oper_func->priority != 5) {
              push(&polish, oper_func->num, oper_func->priority,
                   oper_func->type);
              pop(&oper_func);
            }
          }
        }
        push(&oper_func, line->num, line->priority, line->type);
      }
    }
    if (line->type == Lbracket_1) {
      push(&oper_func, line->num, line->priority, line->type);
    }
    if (line->type == number || line->type == zero || line->type == X) {
      push(&polish, line->num, line->priority, line->type);
    }
    if (line->type == Rbracket_1) {
      if (oper_func != NULL) {
        while (oper_func->type != Lbracket_1) {
          push(&polish, oper_func->num, oper_func->priority, oper_func->type);
          pop(&oper_func);
        }
      }
      pop(&oper_func);
    }
    pop(&line);
  }
  while (oper_func) {
    push(&polish, oper_func->num, oper_func->priority, oper_func->type);
    pop(&oper_func);
  }
  polish = inverse_stack(polish);
  return polish;
}
