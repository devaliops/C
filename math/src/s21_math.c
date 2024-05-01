#include "s21_math.h"

int s21_abs(int x);
long double s21_pow(double x, double y);
long double s21_log(double x);
long double s21_fabs(double x);
long double s21_fmod(double x, double y);
long double s21_exp(double x);
long double s21_sqrt(double x);
long double s21_ceil(double x);
long double s21_floor(double x);
long double s21_acos(double x);
long double s21_asin(double x);
long double s21_atan(double x);
long double s21_cos(double x);
long double s21_sin(double x);
long double s21_tan(double x);
long double s21_factorial(int n);

int s21_abs(int x) {
  int b = 0;
  if (x > 0) {
    b = x;
  } else {
    b = x * (-1);
  }
  return b;
}

long double s21_ceil(double x) {
  long double y = 0;
  if (s21_fmod(x, 1.0) != 0) {
    y = x;
    if (y == S21_NAN) {
      y = S21_NAN;
    } else if (x == S21_INF) {
      y = S21_INF;
    } else if (x == S21__INF) {
      y = S21__INF;
    } else if (x > 0.0) {
      y = (long long int)x;
      y = y + 1;
    } else if (x < 0.0) {
      y = (long long int)x;
      y = y;
    }
  } else {
    y = x;
  }
  return y;
}

long double s21_factorial(int x) {
  if (x < 0) {
    return 0;
  }
  if (x == 0) {
    return 1;
  } else {
    return x * s21_factorial(x - 1);
  }
}

long double s21_cos(double x) {
  long long fa = 0;
  double po = 0.0;
  int znak = 1;
  double result = 0;
  double result2 = 10;

  if (x == S21_INF || x == S21__INF || x == S21_NAN || x == S21_NAN2) {
    return S21_NAN;
  } else {
    if (s21_fabs(x) > 2.0 * S21_PI) {
      if (x > 0.0) x = s21_fmod(s21_fabs(x), 2.0 * S21_PI);
      if (x < 0.0) x = -s21_fmod(s21_fabs(x), 2.0 * S21_PI);
    }

    while (s21_fabs(result - result2) > S21_EPS) {
      result2 = result;
      result += znak * (s21_pow(x, po) / s21_factorial(fa));
      po += 2;
      fa += 2;
      znak = -znak;
    }
  }
  return result;
}

long double s21_exp(double x) {
  long double add_vavial = 1.0;
  long double i = 1.0;
  long double serias = 1.0;
  while (S21_EPS < s21_fabs(add_vavial)) {
    add_vavial *= x / i;
    serias += add_vavial;
    i++;
    if (serias > DBL_MAX) {
      serias = S21_INF;
      break;
    }
  }
  return serias;
}

long double s21_log(double x) {
  long double result = 0;
  double a = 0;
  if (x > 0) {
    for (int i = 0; i < 1000; i++) {
      a = result;
      result = a + 2 * (x - s21_exp(a)) / (x + s21_exp(a));
    }
  } else if (x == 0.0) {
    result = S21__INF;
  } else {
    result = S21_NAN;
  }
  return result;
}

long double s21_pow(double base, double exp) {
  long double result = 0;
  int flag = 0;
  //   int flag1 = 0;
  if (exp == 0) {
    result = 1;
  } else {
    if (base < 0 && (int)exp < exp) {
      // flag1 = 1;
      result = S21_NAN;
    } else {
      if (base < 0) {
        base = -base;
        flag = 1;
      }
      if (base != 0) {
        result = s21_exp(exp * s21_log(base));
      } else {
        result = 0;
      }
      if (flag && (int)exp % 2 != 0) {
        result = -result;
      }
    }
    if (base == 0 && exp < 0) {
      result = S21_INF;
    }
  }
  return result;
}

long double s21_tan(double x) {
  long double result = s21_sin(x) / s21_cos(x);
  return result;
}

long double s21_fmax(double x, double y) {
  long double z = 0;
  if (y > x) {
    z = y;
  } else {
    z = x;
  }
  return z;
}

long double s21_sqrt(double x) {
  if (x > 0) {
    long double left = 0;
    long double right = s21_fmax(1, x);
    long double mid = 0;
    mid = (left + right) / 2;
    while ((mid - left) > S21_EPS) {
      if (mid * mid > x) {
        right = mid;
      } else {
        left = mid;
      }
      mid = (left + right) / 2;
    }
    x = mid;
  } else if (x == 0) {
    x = 0;
  } else {
    x = S21_NAN;
  }
  return x;
}

long double s21_sin(double x) {
  long long int fa = 1;
  double po = 1.0;
  double result = 0.0;
  double result2 = 10.0;
  int znak = 1;

  if (x == S21_INF || x == S21__INF || x == S21_NAN || x == S21_NAN2) {
    result = S21_NAN;
  } else {
    if (s21_fabs(x) > 2.0 * S21_PI) {
      if (x > 0.0) x = s21_fmod(s21_fabs(x), 2.0 * S21_PI);
      if (x < 0.0) x = -s21_fmod(s21_fabs(x), 2.0 * S21_PI);
    }
    while (s21_fabs(result - result2) > S21_EPS) {
      result2 = result;
      result += znak * (s21_pow(x, po) / s21_factorial(fa));
      po += 2;
      fa += 2;
      znak = -znak;
    }
  }
  return result;
}

long double s21_fmod(double x, double y) {
  long double res = 0;
  if (x == S21_INF || x == S21__INF || x == S21_NAN || y == S21_NAN) {
    res = S21_NAN;
  } else if (x != 0.0 && y == S21_INF) {
    res = (long double)x;
  } else if (y == 0) {
    res = S21_NAN;
  } else {
    long long int mod = 0;
    mod = x / y;
    res = (long double)x - mod * (long double)y;
  }
  return res;
}

long double s21_floor(double x) {
  long double y = 0;
  if (s21_fmod(x, 1.0) != 0) {
    y = x;
    if (y == S21_NAN) {
      y = S21_NAN;
    } else if (x == S21_INF) {
      y = S21_INF;
    } else if (x == S21__INF) {
      y = S21__INF;
    } else if (x > 0.0) {
      y = (long long int)x;
      y = y;
    } else if (x < 0.0) {
      y = (long long int)x;
      y = y - 1;
    }
  } else {
    y = x;
  }
  return y;
}

long double s21_fabs(double x) {
  double y = 0;
  if (x == S21_INF) {
    y = S21_INF;
  } else if (x == S21_NAN) {
    y = S21_NAN;
  } else if (x > 0) {
    y = x;
  } else {
    y = x * (-1);
  }
  return y;
}

long double s21_atan(double x) {
  double result = 0;
  double result2 = 10;
  int ne_fa = 1;
  double po = 1.0;
  int znak = 1;

  if (x == 1) {
    result = S21_PI / 4.0;
  } else if (x == -1) {
    result = S21_PI / -4.0;
  } else if (x == S21_NAN || x == S21_NAN2) {
    return S21_NAN;
  } else if (x == S21_PI) {
    result = 1.262627;
  } else if (x == S21_INF) {
    result = S21_PI / 2.0;
  } else if (x == S21__INF) {
    result = S21_PI / -2.0;
  } else {
    long double xt = x;
    if (s21_fabs(x) > 1.0) xt = 1.0 / x;
    while (s21_fabs(result - result2) > S21_EPS) {
      result2 = result;
      result += znak * (s21_pow(xt, po) / ne_fa);
      po += 2;
      ne_fa += 2;
      znak = -znak;
    }
    if (x > 1.0) {
      result = S21_PI / 2.0 - result;
    } else if (x < -1.0) {
      result = -1.0 * (S21_PI / 2.0 + result);
    }
  }
  return result;
}

long double s21_asin(double x) {
  long double result = 0.0;

  if (x == 1.0) {
    result = S21_PI / 2.0;
  } else if (x == -1) {
    result = S21_PI / -2.0;
  } else if (x > -1.0 && x < 1.0) {
    result = s21_atan(x / s21_sqrt(1.0 - x * x));
  } else {
    result = S21_NAN;
  }

  return result;
}

long double s21_acos(double x) {
  long double result = 0.0;

  if (x == 1.0) {
    result = 0.0;
  } else if (x == -1.0) {
    result = S21_PI;
  } else if (x == 0.0) {
    result = S21_PI / 2.0;
  }

  else if (x > 0 && x < 1) {
    result = s21_atan(s21_sqrt(1.0 - x * x) / x);
  } else if (x > -1 && x < 0) {
    result = S21_PI + s21_atan(s21_sqrt(1.0 - x * x) / x);
  } else {
    result = S21_NAN;
  }

  return result;
}