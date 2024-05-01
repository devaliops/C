WWW = -Werror -Wextra -Wall
G = gcc

all : clean s21_math.a test gcov_report

clean: 
	rm -rf *.o *.a *.gch *.gcno *.gcna *.gcda *.info test test_html

test: clean
	$(G) $(WWW) s21_math.c s21_math.h unit_test.c -lcheck -o test

s21_math.a:
	$(G) $(WWW) -c s21_*.c
	ar r s21_math.a *.o
	rm *.o

gcov_report:
	$(G) $(WWW) -fprofile-arcs -ftest-coverage s21_math.c s21_math.h unit_test.c -pthread -lcheck -pthread -lm -o test
	./test
	lcov -t "test_html" -o unit_test.info -c -d .
	genhtml -o test_html unit_test.info
	open test_html/index.html

leaks:
	leaks -atExit -- ./test