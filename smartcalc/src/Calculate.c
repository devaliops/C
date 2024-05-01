#include "SmartCalc.h"

double result(Node_Stack* pol_not, double num) {
  double res = 0;
  double res_1 = 0;
  double res_2 = 0;
  double x_1 = num;
  Node_Stack* number = NULL;
  Node_Stack* oper_func = NULL;
  while (pol_not) {
    if (operator_stack(pol_not->type) || func(pol_not->type)) {
      push(&oper_func, pol_not->num, pol_not->priority, pol_not->type);
    }
    if (pol_not->priority == 0) {
      if (pol_not->type == 0) {
        push(&number, pol_not->num, pol_not->priority, pol_not->type);
      } else if (pol_not->type == zero) {
        push(&number, pol_not->num, pol_not->priority, pol_not->type);
      } else if (pol_not->type == X) {
        push(&number, x_1, pol_not->priority, pol_not->type);
      }
    }
    if (oper_func != NULL) {
      if (operator_stack(oper_func->type)) {
        res_2 = number->num;
        pop(&number);
        res_1 = number->num;
        pop(&number);
        res = calc(res_1, res_2, oper_func->type);
        push(&number, res, 0, 0);
        pop(&oper_func);
      }
    }
    if (oper_func != NULL) {
      if (func(oper_func->type)) {
        res_1 = number->num;
        pop(&number);
        res = calc(res_1, 0, oper_func->type);
        push(&number, res, 0, 0);
        pop(&oper_func);
      }
    }
    pol_not = pol_not->next;
  }
  if (oper_func == NULL) {
    res = number->num;
  }
  free(number);
  free(oper_func);
  return res;
}