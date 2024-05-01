#include "SmartCalc.h"

Node_Stack *push(Node_Stack **head, double num, int priority, type_t type) {
  Node_Stack *tmp = malloc(sizeof(Node_Stack));
  tmp->priority = priority;
  tmp->num = num;
  tmp->type = type;
  tmp->next = *head;
  *head = tmp;
  return *head;
}

void pop(Node_Stack **head) {
  Node_Stack *out;
  out = *head;
  *head = (*head)->next;
  free(out);
}

int number_check(char sym){
  int res = 0;
  if (sym >= '0' && sym <= '9')
  {
    res = 1;
  }
  return res;
}

int operator_stack(type_t type) {
  int res = 0;
  if (type == plus_1) {
    res = 1;
  } else if (type == minus_1) {
    res = 1;
  } else if (type == mult_1) {
    res = 1;
  } else if (type == div_1) {
    res = 1;
  } else if (type == mod_1) {
    res = 1;
  } else if (type == pow_1) {
    res = 1;
  }
  return res;
}

int func(type_t type) {
  int res = 0;
  if (type == cos_1) {
    res = 1;
  } else if (type == sin_1) {
    res = 1;
  } else if (type == tan_1) {
    res = 1;
  } else if (type == acos_1) {
    res = 1;
  } else if (type == asin_1) {
    res = 1;
  } else if (type == atan_1) {
    res = 1;
  } else if (type == sqrt_1) {
    res = 1;
  } else if (type == ln_1) {
    res = 1;
  } else if (type == log_1) {
    res = 1;
  }
  return res;
}

double calc(double value_1, double value_2, type_t type) {
  long double res = 0;
  if (type == 2) {
    res = value_1 + value_2;
  } else if (type == 3) {
    res = value_1 - value_2;
  } else if (type == 4) {
    res = value_1 * value_2;
  } else if (type == 5) {
    res = value_1 / value_2;
  } else if (type == 6) {
    res = fmod(value_1, value_2);
  } else if (type == 7) {
    res = pow(value_1, value_2);
  } else if (type == 8) {
    res = cos(value_1);
  } else if (type == 9) {
    res = sin(value_1);
  } else if (type == 10) {
    res = tan(value_1);
  } else if (type == 11) {
    res = acos(value_1);
  } else if (type == 12) {
    res = asin(value_1);
  } else if (type == 13) {
    res = atan(value_1);
  } else if (type == 14) {
    res = sqrt(value_1);
  } else if (type == 15) {
    res = log(value_1);
  } else if (type == 16) {
    res = log10(value_1);
  }
  return res;
}

Node_Stack *inverse_stack(Node_Stack *line) {
  Node_Stack *tmp = NULL;
  while (line != NULL) {
    push(&tmp, line->num, line->priority, line->type);
    pop(&line);
  }
  return tmp;
}
