#ifndef SMART_CALC_H
#define SMART_CALC_H

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  zero = -1,
  number = 0,
  X = 1,
  plus_1 = 2,
  minus_1 = 3,
  mult_1 = 4,
  div_1 = 5,
  mod_1 = 6,
  pow_1 = 7,
  cos_1 = 8,
  sin_1 = 9,
  tan_1 = 10,
  acos_1 = 11,
  asin_1 = 12,
  atan_1 = 13,
  sqrt_1 = 14,
  ln_1 = 15,
  log_1 = 16,
  Lbracket_1 = 17,
  Rbracket_1 = 18
} type_t;

typedef struct Node_t {
  double num;
  int priority;
  type_t type;
  struct Node_t *next;
} Node_Stack;

Node_Stack *push(Node_Stack **head, double num, int priority, type_t type);
void pop(Node_Stack **head);
int peek(const Node_Stack *head);

Node_Stack *parcer(const char *str);
Node_Stack *polish_notation(Node_Stack *line);
void pol_not_char(Node_Stack *line, char *str);
double result(Node_Stack* pol_not, double num);
double calc(double value_1, double value_2, type_t type);

Node_Stack *inverse_stack(Node_Stack *line);
int operator_stack(type_t type);
int func(type_t type);
int Priority(int type_oper);
int strlen_stack(Node_Stack *line);
int number_check(char sym);
int validation(const char *str);
void operator_check_str(const char *str, int *error, int i);
void func_check_str(const char *str, int *error, int *i);
#endif