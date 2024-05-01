#include "SmartCalc.h"

void pol_not_char(Node_Stack *line, char *tmp) {
  char str[1000] = {0};
  while (line) {
    if (line->type == number) {
      sprintf(str, "%.1lf", line->num);
      strcat(tmp, str);
      strcat(tmp, " ");
    }

    if (line->type == X) {
      strcat(tmp, "x ");
    }

    if (line->type == plus_1) {
      strcat(tmp, "+ ");
    }

    if (line->type == minus_1) {
      strcat(tmp, "- ");
    }

    if (line->type == mult_1) {
      strcat(tmp, "* ");
    }

    if (line->type == div_1) {
      strcat(tmp, "/ ");
    }

    if (line->type == mod_1) {
      strcat(tmp, "mod ");
    }

    if (line->type == pow_1) {
      strcat(tmp, "^ ");
    }

    if (line->type == cos_1) {
      strcat(tmp, "cos ");
    }

    if (line->type == sin_1) {
      strcat(tmp, "sin ");
    }

    if (line->type == tan_1) {
      strcat(tmp, "tan ");
    }

    if (line->type == acos_1) {
      strcat(tmp, "acos ");
    }

    if (line->type == asin_1) {
      strcat(tmp, "asin ");
    }

    if (line->type == atan_1) {
      strcat(tmp, "atan ");
    }

    if (line->type == sqrt_1) {
      strcat(tmp, "sqrt ");
    }

    if (line->type == ln_1) {
      strcat(tmp, "ln ");
    }

    if (line->type == log_1) {
      strcat(tmp, "log ");
    }
    pop(&line);
  }
}
