from compiler import *
from subprocess import *
from datetime import *
from argparse import *
import os

parser = ArgumentParser()

subparsers = parser.add_subparsers(title='subcommands', description='valid subcommands', help='additional help')
parser.add_argument("-c", "--code", help="Compile this code directly", type=str, default="")

all_parser = subparsers.add_parser('all', help='Run all tests')
skip_parser = subparsers.add_parser('skip', help='Skip sections of tests or specific tests')
only_parser = subparsers.add_parser('only', help='Run specific sections of tests or specific tests')

skip_parser.add_argument("-s", "--skip-sections", help="Skip these tests sections", type=int, nargs='*', default=[])
skip_parser.add_argument("-t", "--skip-tests", help="Skip these (specific) tests", type=int, nargs='*', default=[])

only_parser.add_argument("-s", "--only-sections", help="Only run these test sections", type=int, nargs='*', default=[])
only_parser.add_argument("-t", "--only-tests", help="Only run these tests", type=int, nargs='*', default=[])

args = parser.parse_args()

tmpSourceFile = 'tester_tmp.scm'
srcNoExt = tmpSourceFile.replace('.scm', '')
tmpTargetFile = 'tester_tmp.asm'

tests = []
#tests.append(('ApplicTP - will cause an infinite loop that doesn\'t crash',))
#tests.append(("((lambda (x) (x x)) (lambda (x) (x x)))", 'infinite loop', 'ApplicTP frame replacement'))

tests.append(('append',))
tests.append(("(append)", "()", 'append with no args'))
tests.append(("(append '(1 2))", "(1 . (2 . ()))", 'append with one constant list arg'))
tests.append(("(append (list 1 2))", "(1 . (2 . ()))", 'append with one dynamic list arg'))
tests.append(("(append '(1 2) '(3 4) (list 5 6))", "(1 . (2 . (3 . (4 . (5 . (6 . ()))))))", 'append with multiple args'))
tests.append(("(append 5)", "5", 'append with one, non-list arg'))
tests.append(("(append '() 5)", "5", 'creating improper list with append'))
tests.append(("(append '(1 2) 5)", "(1 . (2 . 5))", 'creating improper list with append'))

tests.append(('apply',))
tests.append(("(apply + '(3 4 5 6))", "18", 'apply on + and a const list'))
tests.append(("(apply append '((1 2) (3 4)))", "(1 . (2 . (3 . (4 . ()))))", 'apply on append and multiple lists'))

tests.append(('map',))
tests.append(("(map (lambda (x . z) (+ x 10)) '(1 2 3))", "(11 . (12 . (13 . ())))", 'map on a lambda-opt'))
tests.append(("(map (lambda x x) '(1))", "((1 . ()) . ())", 'map on a lambda-var'))
tests.append(("(map (lambda (x) x) '(1))", "(1 . ())", 'map on a lambda-simple'))
tests.append(("(map (lambda (x y) (- x y)) '(1) '(2))", "(-1 . ())", 'map on a lambda-simple with two arg lists'))
tests.append(("(map + '(1 2 3) '(2 4 6) '(5 10 15))", "(8 . (16 . (24 . ())))", 'map on a lambda-simple with three arg lists'))

tests.append(('multiple expressions',))
tests.append(("(define x 10)", "", 'don\'t print #void'))
tests.append(("(define x 5) x", "5", 'multiple expressions with define'))
tests.append(("(define x 5) (define x 6) x", "6", 'multiple expressions with define overwriting'))
tests.append(("6 5", "6\n5", 'multiple expressions in different lines'))

tests.append(('lambda-simple',))
tests.append(("((lambda (x) x) 5)", '5', 'identity lambda on integer'))

tests.append(('lambda-opt',))
tests.append(("((lambda (x . y) y) 1)", '()', 'lambda opt with one arg'))
tests.append(("((lambda (x . y) y) 1 2)", '(2 . ())', 'lambda opt with multiple args'))
tests.append(("((lambda (x . y) y) 1 2 3)", '(2 . (3 . ()))', 'lambda opt with multiple args'))
tests.append(("((lambda (x . y) x) 1)", '1', 'lambda opt with one arg'))
tests.append(("((lambda (x . y) x) 1 2)", '1', 'lambda opt with multiple args'))
tests.append(("((lambda (x y . z) y) 1 2 3)", '2', 'lambda opt with multiple args'))

tests.append(('lambda-var',))
tests.append(("((lambda x x))", '()', 'lambda var with no args'))
tests.append(("((lambda x x) 1)", '(1 . ())', 'lambda var with one arg'))
tests.append(("((lambda x x) 1 2 3 4 5)", '(1 . (2 . (3 . (4 . (5 . ())))))', 'lambda var with multiple args'))

tests.append(('nested quotes',))
tests.append(("(+ '1 2)", "3", 'quote and computation'))
tests.append(("'1", "1", 'quote for integer'))
tests.append(("'(1 2 '3)", "(1 . (2 . ((QUOTE . (3 . ())) . ())))", 'nested quoted list'))
tests.append(("`(1 2 ,@'(3))", "(1 . (2 . (3 . ())))", 'quasi-quote expansions'))

tests.append(('un-quoting',))
tests.append(("(append '(1 2) `,'(3))", "(1 . (2 . (3 . ())))", 'un-quoting and appending a list'))
tests.append(("(append '(1) `(2 ,@'(3 4)))", "(1 . (2 . (3 . (4 . ()))))", 'un-quoting and appending a list'))

tests.append(('or',))
tests.append(("(or)", '#f', 'or with no params'))
tests.append(("(or 2 3 4)", '2', 'OR with no #f args'))
tests.append(("(or #f 3 4)", '3', 'OR with first arg as #f'))
tests.append(("(or #f #f)", '#f', 'OR with all args as #f'))
tests.append(("(or #t #f)", '#t', 'OR with first arg as #t'))

tests.append(('and',))
tests.append(("(and)", '#t', 'and with no params'))
tests.append(("(and 1)", '1', 'and with one integer param'))
tests.append(("(and 1 #f)", '#f', 'and with #f'))

tests.append(('null?',))
tests.append(("(null? '())", '#t', 'NULL? on empty static list'))
tests.append(("(null? (list))", '#t', 'NULL? on empty dynamic list'))
tests.append(("(null? '(2))", '#f', 'NULL? on NON empty static list'))
tests.append(("(null? (list 3))", '#f', 'NULL? on NON empty dynamic list'))

tests.append(('number?',))
tests.append(("(number? 5)", '#t', 'NUMBER? on integer constant'))
tests.append(("(number? ((lambda () 6)))", '#t', 'NUMBER? on result of a lambda applic (int)'))
tests.append(("(number? #t)", '#f', 'NUMBER? on boolean'))

tests.append(('string-ref',))
tests.append(("(string-ref \"Hello World!\" 0)", '#\\H', 'string-ref first character'))
tests.append(("(string-ref \"Hello World!\" 6)", '#\\W', 'string-ref middle character'))

tests.append(('string-length',))
tests.append(("(string-length \"Hello World!\")", '12', 'string-length on a string'))
tests.append(("(string-length \"\")", '0', 'string-length on an empty string'))

tests.append(('zero?',))
tests.append(("(zero? 0)", '#t', 'ZERO? on a 0 constant'))
tests.append(("(zero? 5)", '#f', 'ZERO? on a 5 constant'))

tests.append(('char?',))
tests.append(("(char? #\\b)", '#t', 'CHAR? on a char constant'))
tests.append(("(char? \"hi\")", '#f', 'CHAR? on a string constant'))

tests.append(('string?',))
tests.append(("(string? \"some string\")", '#t', 'STRING? on a string constant'))
tests.append(("(string? 'hi)", '#f', 'STRING? on a symbol'))

tests.append(('procedure?',))
tests.append(("(procedure? (lambda (x) 6))", '#t', 'PROCEDURE? on lambda simple'))
tests.append(("(procedure? (lambda (x . y) 6))", '#t', 'PROCEDURE? on lambda opt'))
tests.append(("(procedure? (lambda x 6))", '#t', 'PROCEDURE? on lambda variadic'))
tests.append(("(procedure? car)", '#t', 'PROCEDURE? on a global procedure'))
tests.append(("(procedure? ((lambda x 6) 8))", '#f', 'PROCEDURE? on applic'))

tests.append(('pair?',))
tests.append(("(pair? '(2 . 3))", '#t', 'PAIR? on a constant pair'))
tests.append(("(pair? (cons 2 3))", '#t', 'PAIR? on a dynamic pair'))
tests.append(("(pair? '(5))", '#t', 'PAIR? on a constant list with one element'))
tests.append(("(pair? '(5 6))", '#t', 'PAIR? on a constant list with two elements'))
tests.append(("(pair? '())", '#f', 'PAIR? on the empty list'))
tests.append(("(pair? 8)", '#f', 'PAIR? on an integer constant'))

tests.append(('boolean?',))
tests.append(("(boolean? #t)", '#t', 'BOOLEAN? on constant #t'))
tests.append(("(boolean? #f)", '#t', 'BOOLEAN? on constant #f'))
tests.append(("(boolean? 7)", '#f', 'BOOLEAN? on an integer constant'))

tests.append(('car',))
tests.append(("(car '(3 5))", '3', 'CAR on a constant list'))
tests.append(("(car (list \"hello\" \"world\"))", '"hello"', 'CAR on a dynamic list'))

tests.append(('cdr',))
tests.append(("(cdr '(3 5))", '(5 . ())', 'CDR on a constant list'))
tests.append(("(cdr (list \"hello\" \"world\"))", '("world" . ())', 'CDR on a dynamic list'))

tests.append(('cons',))
tests.append(("(cons 1 2)", '(1 . 2)', 'CONS on integer constants'))
tests.append(("(cons 1 (cons 2 '()))", '(1 . (2 . ()))', 'CONS recursive to create a list'))

tests.append(('remainder',))
tests.append(("(remainder 8 10)", '8', 'remainder for x > y (positive)'))
tests.append(("(remainder -8 10)", '-8', 'remainder for x > y (negative)'))
tests.append(("(remainder 8 3)", '2', 'remainder for x < y (positive)'))
tests.append(("(remainder -8 3)", '-2', 'remainder for x < y (negative)'))

tests.append(('>',))
tests.append(("(> 3 2 1)", '#t', '> for x > y > z (integers)'))
tests.append(("(> 3 2 2)", '#f', '> for x > y = z (integers)'))
tests.append(("(> 5/2 4/3 -1/2)", '#t', '> for x > y > z (fractions)'))
tests.append(("(> 5/2 5/2 4/3)", '#f', '> for x = y > z (fractions)'))
tests.append(("(> 5 4/3 -3 -20/3 -50)", '#t', '> for x > y > z (mixed)'))
tests.append(("(> 250 3 3)", '#f', '> for x > y = z (mixed)'))

tests.append(('<',))
tests.append(("(< 1 2 3)", '#t', '< for x < y < z (integers)'))
tests.append(("(< 1 2 2)", '#f', '< for x < y = z (integers)'))
tests.append(("(< -1/2 4/3 5/2)", '#t', '< for x < y < z (fractions)'))
tests.append(("(< 4/3 5/2 5/2)", '#f', '< for x = y < z (fractions)'))
tests.append(("(< -50 -20/3 -3 4/3 5)", '#t', '< for x < y < z (mixed)'))
tests.append(("(< 3 3 250)", '#f', '< for x < y = z (mixed)'))

tests.append(('=',))
tests.append(("(= 2 2)", '#t', '= for 2 constant args (integers)'))
tests.append(("(= 2/4 2/4)", '#t', '= for 2 constant args (fractions)'))
tests.append(("(= 4/2 2)", '#t', '= for 2 constant args (mixed)'))
tests.append(("(= 2 4/2)", '#t', '= for 2 constant args (mixed) (reverse of previous)'))
tests.append(("(= 4/2 2 8/4 16/8 2)", '#t', '= for 5 constant args (mixed)'))
tests.append(("(= 4/2 2 8/4 16/8 2 0)", '#f', '= for 6 constant args (mixed)'))

tests.append(('+',))
tests.append(("(+)", '0', '+ for no args'))
tests.append(("(+ 2)", '2', '+ for 1 constant args (integers)'))
tests.append(("(+ 2 10)", '12', '+ for 2 constant args (integers)'))
tests.append(("(+ 2 -10)", '-8', '+ for 2 constant args (integers)'))
tests.append(("(+ 4/16)", '1/4', '+ for 1 constant args (fractions) (with minimization)'))
tests.append(("(+ 1/2 1/4)", '3/4', '+ for 2 constant args (fractions)'))
tests.append(("(+ 1/2 -1/4)", '1/4', '+ for 2 constant args (fractions)'))
tests.append(("(+ 1/2 -2 3/2 5 10)", '15', '+ for 5 constant args (mixed)'))

tests.append(('*',))
tests.append(("(*)", '1', '* for no args'))
tests.append(("(* 2)", '2', '* for 1 constant args (integers)'))
tests.append(("(* 2 10)", '20', '* for 2 constant args (integers)'))
tests.append(("(* 2 -10)", '-20', '* for 2 constant args (integers)'))
tests.append(("(* 4/16)", '1/4', '* for 1 constant args (fractions) (with minimization)'))
tests.append(("(* 1/2 1/4)", '1/8', '* for 2 constant args (fractions)'))
tests.append(("(* 1/2 -1/4)", '-1/8', '* for 2 constant args (fractions)'))
tests.append(("(* 1/2 -2 3/2 5 10)", '-75', '* for 5 constant args (mixed)'))

tests.append(('char->integer',))
tests.append((r"(char->integer #\a)", '97', 'get ascii value of a'))

tests.append(('integer->char',))
tests.append(("(integer->char 97)", r"#\a", 'get char of ascii value 97'))

tests.append(('symbols',))
tests.append(("'hi", 'hi', 'return a symbol (in uppercase!)'))
tests.append(("(string->symbol \"hi\")", 'hi', 'return a symbol from a string representation'))
tests.append(("(symbol->string 'hi)", '"hi"', 'return a string from a symbol representation'))
tests.append(("(eq? (string->symbol \"hi\") 'hi)", '#t', 'eq between dynamic retrieval of a symbol from string and the symbol'))
tests.append(("(eq? (symbol->string 'hi) \"hi\")", '#t', 'eq between dynamic retrieval of a string from symbol and the string'))
tests.append(("(eq? (string->symbol (symbol->string 'hi)) 'hi)", '#t', 'symbol->string and string->symbol invariant test'))
tests.append(("(eq? 'hi \"hi\")", '#f', 'a symbol and the matching string are different objects'))

tests.append(('string',))
tests.append((r"(make-string 0 #\a)", '""', 'create an empty string'))
tests.append((r"(make-string 4 #\a)", '"aaaa"', 'create a repetitive string'))
tests.append(("(string-length \"aaaa\")", '4', 'string-length of "aaaa"'))
tests.append(("(string-length \"\")", '0', 'string-length of ""'))
tests.append(("(string-ref \"abcd\" 0)", r"#\a", 'string-ref 0 on "abcd"'))
tests.append(("(string-ref \"\n\" 0)", r"#\newline", r"string-ref 0 on \"\\n\""))

tests.append(('vector',))
tests.append(("(vector)", '#0()', 'create an empty vector'))
tests.append(("(vector 1)", '#1(1)', 'create a vector with one element'))
tests.append(("(vector 1 2 3 4 5 6)", '#6(1 2 3 4 5 6)', 'create a vector with multiple elements'))
tests.append(("(vector-ref '#(1 2 3 4 5 6) 0)", '1', 'retrieve vector first element (static vector)'))
tests.append(("(vector-ref '#(1 2 3 4 5 6) 5)", '6', 'retrieve vector last element (static vector)'))
tests.append(("(vector-length '#(1 2 3 4 5 6))", '6', 'retrieve vector length (static vector)'))
tests.append(("(vector-ref (vector 1 2 3 4 5 6) 0)", '1', 'retrieve vector first element (dynamic vector)'))
tests.append(("(vector-ref (vector 1 2 3 4 5 6) 5)", '6', 'retrieve vector last element (dynamic vector)'))
tests.append(("(vector-length (vector 1 2 3 4 5 6))", '6', 'retrieve vector length (dynamic vector)'))
tests.append(("(make-vector 5)", '#5(0 0 0 0 0)', 'fill a vector of size 5 with no specific value'))
tests.append(("(make-vector 0 8)", '#0()', 'fill a vector of size 0'))
tests.append(("(make-vector 4 8)", '#4(8 8 8 8)', 'fill a vector of size 4 with the integer 8'))

tests.append(('list',))
tests.append(("(list)", '()', 'create an empty list'))
tests.append(("(eq? '() (list))", '#t', 'empty list should be a singleton'))
tests.append(("(list 1)", '(1 . ())', 'create a list with one element'))
tests.append(("(list 1 2 3 4 5 6)", '(1 . (2 . (3 . (4 . (5 . (6 . ()))))))', 'create a list with multiple elements'))

tests.append(('eq? implementation',))
tests.append(("(eq? '(7 8) '(7 8))", '#t', 'same constant lists should be eq'))
tests.append(("(eq? '() (cdr '(5)))", '#t', 'cdr of a one-element list should be nil'))
tests.append(("(eq? 0 0)", '#t', 'eq of two integers'))
tests.append(("(eq? #f #f)", '#t', 'eq of two booleans'))
tests.append(("(eq? 1/2 1/2)", '#t', 'eq of two fractions'))
tests.append(("(eq? '(1 2) '(1 2))", '#t', 'eq of two constant lists'))
tests.append(("(eq? (list 1 2) (list 1 2))", '#f', 'eq of two dynamic lists'))
tests.append(("(eq? car car)", '#t', 'eq of two (same) procedures'))
tests.append(("(eq? car cdr)", '#f', 'eq of two (not the same) procedures'))
tests.append(("(eq? 3 (+ 1 2))", '#t', 'same integers, one constant and one dynamic'))
tests.append((r"(eq? #\a #\a)", '#t', 'eq of two constant chars'))
tests.append(("(eq? (integer->char 60) (integer->char 60))", '#t', 'eq of two dynamic chars'))
tests.append(("(eq? (string->symbol (symbol->string 'hi)) 'hi)", '#t', 'eq of two dynamic symbols (case insensitive)'))
tests.append(("(eq? (string->symbol (symbol->string 'hi)) 'Hi)", '#t', 'eq of two dynamic symbols (case sensitive)'))
tests.append(("(eq? (vector-ref '#(1 2 3) 0) 1)", '#t', 'constant tagging of elements inside a vector'))
tests.append(("(eq? (car '(1 2 3)) 1)", '#t', 'constant tagging of elements inside a constant list'))
tests.append(("(eq? (car (list 1 2 3)) 1)", '#t', 'constant tagging of elements inside a dynamic list'))

tests.append(('letrec',))
tests.append(("""
(letrec ((factorial (lambda (x)
                      (if (< x 1)
                      1
                      (* (factorial (- x 1)) x)))))
  (factorial 5))
""", "120", 'letrec factorial'))

tests.append(('let-star',))
tests.append(("""
(let* ((Yag
    (lambda fs
      (let ((ms (map
                  (lambda (fi)
                      (lambda ms
                            (apply fi (map (lambda (mi)
                                                  (lambda args
                                                           (apply (apply mi ms) 
args))) ms))))
                  fs)))
        (apply (car ms) ms))))
         )
    (letrec ((tail (lambda (n) (if (pair? n) (if (null? (cdr n)) (car n) (tail (
cdr n))) n)))) (tail '(1 2 3 4))))
""", "4", 'let-star with Yag'))


tests.append(('Crazy lambda simple & applic - increase ram to Mega(100)',))
tests.append(("""
(((((lambda (a)
      (lambda (b)
        (((lambda (a) (lambda (b) ((a b) (lambda (x) (lambda (y) y)))))
    ((lambda (n)
       ((n (lambda (x) (lambda (x) (lambda (y) y))))
        (lambda (x) (lambda (y) x))))
     (((lambda (a)
         (lambda (b)
     ((b (lambda (n)
           ((lambda (p) (p (lambda (a) (lambda (b) b))))
      ((n (lambda (p)
            (((lambda (a)
          (lambda (b) (lambda (c) ((c a) b))))
        ((lambda (n)
           (lambda (s)
             (lambda (z) (s ((n s) z)))))
         ((lambda (p)
            (p (lambda (a) (lambda (b) a))))
          p)))
             ((lambda (p)
          (p (lambda (a) (lambda (b) a))))
        p))))
       (((lambda (a)
           (lambda (b) (lambda (c) ((c a) b))))
         (lambda (x) (lambda (y) y)))
        (lambda (x) (lambda (y) y)))))))
      a)))
       a)
      b)))
   ((lambda (n)
      ((n (lambda (x) (lambda (x) (lambda (y) y))))
       (lambda (x) (lambda (y) x))))
    (((lambda (a)
        (lambda (b)
    ((b (lambda (n)
          ((lambda (p) (p (lambda (a) (lambda (b) b))))
           ((n (lambda (p)
           (((lambda (a)
         (lambda (b) (lambda (c) ((c a) b))))
             ((lambda (n)
          (lambda (s)
            (lambda (z) (s ((n s) z)))))
        ((lambda (p)
           (p (lambda (a) (lambda (b) a))))
         p)))
            ((lambda (p)
         (p (lambda (a) (lambda (b) a))))
             p))))
      (((lambda (a)
          (lambda (b) (lambda (c) ((c a) b))))
        (lambda (x) (lambda (y) y)))
       (lambda (x) (lambda (y) y)))))))
     a)))
      b)
     a)))))
    ((lambda (n)
       ((lambda (p) (p (lambda (a) (lambda (b) b))))
  ((n (lambda (p)
        (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
    ((lambda (n) (lambda (s) (lambda (z) (s ((n s) z)))))
     ((lambda (p) (p (lambda (a) (lambda (b) a)))) p)))
         (((lambda (a)
       (lambda (b)
         ((b (a (lambda (a)
            (lambda (b)
        ((a (lambda (n)
              (lambda (s)
          (lambda (z) (s ((n s) z))))))
         b)))))
          (lambda (x) (lambda (y) y)))))
     ((lambda (p) (p (lambda (a) (lambda (b) a)))) p))
    ((lambda (p) (p (lambda (a) (lambda (b) b)))) p)))))
   (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
     (lambda (x) x))
    (lambda (x) x)))))
     (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
   (((lambda (a)
       (lambda (b)
   ((b (a (lambda (a)
      (lambda (b)
        ((a (lambda (n)
        (lambda (s) (lambda (z) (s ((n s) z))))))
         b)))))
    (lambda (x) (lambda (y) y)))))
     (((lambda (a)
   (lambda (b)
     ((b (a (lambda (a)
        (lambda (b)
          ((a (lambda (n)
          (lambda (s) (lambda (z) (s ((n s) z))))))
           b)))))
      (lambda (x) (lambda (y) y)))))
       ((lambda (x) (lambda (y) (x (x (x y)))))
  (lambda (x) (lambda (y) (x (x y))))))
      (lambda (x) (lambda (y) (x (x (x y)))))))
    (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
  #t)
 #f)
""", '#t', '(Crazy Mayer\'s test) Ackerman + Fibonacci + Factorial together'))

tests.append(('algorithms',))
tests.append(("""
(define pivot (lambda (l)
  (cond ((null? l) 'done)
        ((null? (cdr l)) 'done)
        ((<= (car l) (cadr l)) (pivot (cdr l)))
        (#t (car l))
  )
))

(define partition (lambda (piv l p1 p2)
  (if (null? l) (list p1 p2)
     (if (< (car l) piv) (partition piv (cdr l) (cons (car l) p1) p2)
   (partition piv (cdr l) p1 (cons (car l) p2))))))

(define (quicksort l)
 (let ((piv (pivot l)))
   (if (eq? piv 'done) l
     (let ((parts (partition piv l '() '())))
       (append (quicksort (car parts))
               (quicksort (cadr parts)))))))

(quicksort '(3 8 5 4 8 2 4 1 9 4))
""", '(1 . (2 . (3 . (4 . (4 . (4 . (5 . (8 . (8 . (9 . ()))))))))))', 'quicksort'))

if (hasattr(args, 'code')) and args.code:
  tests = []
  tests.append(('manual code execution',))
  tests.append((args.code, 'something', 'Manual Code'))

start_time = datetime.now()
testsCount = 0
skippedTestsCount = 0
failedTestsCount = 0
testsSectionsCount = 0
failedTestsSummary = []

test = 0
section = 0
for t in tests:
  if (len(t) == 1):
    testsSectionsCount += 1
    section = testsSectionsCount
    print('----------------')
    print("[%d] %s" % (section, t[0]), end="")
    if (hasattr(args, 'skip_sections') and section in args.skip_sections)\
        or (hasattr(args, 'only_sections') and not section in args.only_sections):
      print(' (skipped)')
    else:
      print()

    print('----------------')
    continue

  testsCount += 1
  test = testsCount

  if (hasattr(args, 'skip_sections') and section in args.skip_sections)\
      or (hasattr(args, 'only_sections') and\
          not section in args.only_sections and\
          not test in args.only_tests):
    skippedTestsCount += 1
    continue

  scheme_code = t[0]
  expected_output = t[1]
  test_description = t[2]

  print("%d. %s => " % (test, test_description), end="")

  if (hasattr(args, 'skip_tests') and test in args.skip_tests)\
      or (hasattr(args, 'only_tests') and\
          not section in args.only_sections and\
          not test in args.only_tests):
    print("Skipped")
    continue

  with open(tmpSourceFile, 'w') as fd:
    fd.write(scheme_code)

  compile_scheme_file(tmpSourceFile, tmpTargetFile)
  output = getoutput('make ' + srcNoExt + ' > /dev/null && ./' + srcNoExt)

  if str(output).lower() == str(expected_output).lower():
    print("Success")
  else:
    failedTestStr = "%d. Test: %s\nGot:\n%s\nExpected:\n%s" % (test, scheme_code, output, expected_output)
    failedTestsSummary.append(failedTestStr)
    print("Failed!\n" + failedTestStr)
    print("(python3 tester_official.py only -t %d to re-run specific test)" % (test))
    failedTestsCount += 1

end_time = datetime.now()

print()

if failedTestsCount == 0:
  print("All tests passed!", end="")
else:
  print("%d tests failed. (out of %d)" % (failedTestsCount, testsCount), end="")

if skippedTestsCount > 0:
  print(" (%d skipped)" % (skippedTestsCount))
else:
  print()

if failedTestsCount != 0:
  print("Failed tests summary:")
  for summary in failedTestsSummary:
    print(summary)
  print("('python3 tester_official.py only -t test_id' to re-run specific test)")

execution_time = end_time - start_time
print("Executed in %d seconds." % (execution_time.total_seconds()))

try:
  os.remove(tmpSourceFile)
  os.remove(tmpTargetFile)
  os.remove(srcNoExt)
except OSError:
  pass
