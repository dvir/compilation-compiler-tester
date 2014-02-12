from compiler import *
from subprocess import *
from datetime import *
from argparse import *
import os



def main(tests):
  parser = ArgumentParser()
  
  subparsers = parser.add_subparsers(title='subcommands', description='valid subcommands', help='additional help')
  parser.add_argument("-c", "--code", help="Compile this code directly", type=str, default="")
  parser.add_argument("-l", "--list", help="Show a list of available sections", action="store_true")
  
  all_parser = subparsers.add_parser('all', help='Run all tests')
  skip_parser = subparsers.add_parser('skip', help='Skip sections of tests or specific tests')
  only_parser = subparsers.add_parser('only', help='Run specific sections of tests or specific tests')
  
  skip_parser.add_argument("-s", "--skip-sections", help="Skip these tests sections", type=int, nargs='*', default=[])
  skip_parser.add_argument("-t", "--skip-tests", help="Skip these (specific) tests", type=int, nargs='*', default=[])
  
  only_parser.add_argument("-s", "--only-sections", help="Only run these test sections", type=int, nargs='*', default=[])
  only_parser.add_argument("-t", "--only-tests", help="Only run these tests", type=int, nargs='*', default=[])
  
  args = parser.parse_args()

  if args.list:
    sections = filter(lambda x: len(x) == 1, tests)
    for i, j in enumerate(sections):
      print("{0: 3} - {1}".format(i + 1, j[0]))

    return
  
  tmpSourceFile = 'tester_tmp.scm'
  srcNoExt = tmpSourceFile.replace('.scm', '')
  tmpTargetFile = 'tester_tmp.asm'
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
  
    if type(expected_output) is list and str(output).lower() in expected_output \
    			or str(output).lower() == str(expected_output).lower():
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



tests = []
#tests.append(('ApplicTP - will cause an infinite loop that doesn\'t crash',))
#tests.append(("((lambda (x) (x x)) (lambda (x) (x x)))", 'infinite loop', 'ApplicTP frame replacement'))

### append
tests.append(('append',))
tests.append(("(append)", "()", 'append with no args'))
tests.append(("(append '(1 2))", "(1 . (2 . ()))", 'append with one constant list arg'))
tests.append(("(append (list 1 2))", "(1 . (2 . ()))", 'append with one dynamic list arg'))
tests.append(("(append '(1 2) '(3 4) (list 5 6))", "(1 . (2 . (3 . (4 . (5 . (6 . ()))))))", 'append with multiple args'))
tests.append(("(append 5)", "5", 'append with one, non-list arg'))
tests.append(("(append '() 5)", "5", 'creating improper list with append'))
tests.append(("(append '(1 2) 5)", "(1 . (2 . 5))", 'creating improper list with append'))
tests.append(("(append '(401) '(402))", "(401 . (402 . ()))", 'append two quoted lists'))

### apply
tests.append(('apply',))
tests.append(("(apply + '(3 4 5 6))", "18", 'apply on + and a const list'))
tests.append(("(apply append '((1 2) (3 4)))", "(1 . (2 . (3 . (4 . ()))))", 'apply on append and multiple lists'))
tests.append(("(apply cons 1 2 '())", "(1 . 2)", 'apply cons on args and nil list'))
tests.append(("(apply cons 1 '(2))", "(1 . 2)", 'apply cons on an arg and non empty list'))
tests.append(("(apply (lambda (x) ((lambda (x) x) 5)) 3 '())", '5', 'apply a nested lamda'))
tests.append(("(apply (lambda (x) ((lambda () x) )) 3 '())", '3', 'apply a nested lamda'))
tests.append(("(apply (lambda (x y) (apply cons x y '())) '(1 2))", "(1 . 2)", 'apply a nested apply'))

### map
tests.append(('map',))
tests.append(("(map (lambda (x . z) (+ x 10)) '(1 2 3))", "(11 . (12 . (13 . ())))", 'map on a lambda-opt'))
tests.append(("(map (lambda x x) '(1))", "((1 . ()) . ())", 'map on a lambda-var'))
tests.append(("(map (lambda (x) x) '(1))", "(1 . ())", 'map on a lambda-simple'))
tests.append(("(map (lambda (x y) (- x y)) '(1) '(2))", "(-1 . ())", 'map on a lambda-simple with two arg lists'))
tests.append(("(map + '(1 2 3) '(2 4 6) '(5 10 15))", "(8 . (16 . (24 . ())))", 'map on a lambda-simple with three arg lists'))
tests.append(("(map cons '(13 40) '(37 4))", "((13 . 37) . ((40 . 4) . ()))", 'map on cons'))
tests.append(("(map (lambda (x) (map zero? x)) '((1)))", "((#f . ()) . ())", 'nested nasty map'))

### multiple expression
tests.append(('multiple expressions',))
tests.append(("(define x 10)", "", 'don\'t print #void'))
tests.append(("(define x 5) x", "5", 'multiple expressions with define'))
tests.append(("(define x 5) (define x 6) x", "6", 'multiple expressions with define overwriting'))
tests.append(("6 5", "6\n5", 'multiple expressions in different lines'))

### lambda simple
tests.append(('lambda-simple',))
tests.append(("((lambda () 404))", '404', 'lambda simple with no args'))
tests.append(("((lambda (x) x) 5)", '5', 'identity lambda on integer'))
tests.append(("(((lambda () (lambda () 1337))))", '1337', 'nested lambda simple'))
tests.append(("(((lambda () (lambda () (if 1 2 3 ) ))))", '2', 'nested lambda simple with if'))
tests.append(("(if #t ((lambda (x y) x) 404 1667) )", '404', 'lambda in if'))
tests.append(("(if #t ((lambda (x y) y) 1337 202) )", '202', 'lambda in if with two params'))

### lambda opt
tests.append(('lambda-opt',))
tests.append(("((lambda (x . y) y) 1)", '()', 'lambda opt with one arg'))
tests.append(("((lambda (x . y) y) 1 2)", '(2 . ())', 'lambda opt with multiple args'))
tests.append(("((lambda (x . y) y) 1 2 3)", '(2 . (3 . ()))', 'lambda opt with multiple args'))
tests.append(("((lambda (x . y) x) 1)", '1', 'lambda opt with one arg'))
tests.append(("((lambda (x . y) x) 1 2)", '1', 'lambda opt with multiple args'))
tests.append(("((lambda (x y . z) y) 1 2 3)", '2', 'lambda opt with multiple args'))

### lambda var
tests.append(('lambda-var',))
tests.append(("((lambda x x))", '()', 'lambda var with no args'))
tests.append(("((lambda x x) 1)", '(1 . ())', 'lambda var with one arg'))
tests.append(("((lambda x x) 1 2 3 4 5)", '(1 . (2 . (3 . (4 . (5 . ())))))', 'lambda var with multiple args'))

### nested quotes
tests.append(('nested quotes',))
tests.append(("(+ '1 2)", "3", 'quote and computation'))
tests.append(("'1", "1", 'quote for integer'))
tests.append(("'(1 2 '3)", "(1 . (2 . ((QUOTE . (3 . ())) . ())))", 'nested quoted list'))
tests.append(("`(1 2 ,@'(3))", "(1 . (2 . (3 . ())))", 'quasi-quote expansions'))

### un quoting
tests.append(('un-quoting',))
tests.append(("(append '(1 2) `,'(3))", "(1 . (2 . (3 . ())))", 'un-quoting and appending a list'))
tests.append(("(append '(1) `(2 ,@'(3 4)))", "(1 . (2 . (3 . (4 . ()))))", 'un-quoting and appending a list'))

tests.append(("quasi-quote",))
tests.append(("`#(1 2 3)",'#3(1 2 3)','qq list->vector'))
tests.append(("`#(+ 1 2)",'#3(+ 1 2)','qq list->vector'))
tests.append(("`(,@(+ 3 3))",'6','qq + un-quoting+application'))
tests.append(("(symbol? (vector-ref `#(+ 1 2) 0))",'#t','qq make sure we create a symbol'))
tests.append(("`#(10 5 ,(+ 4) ,@(map + '(16 9)) 8)",'#6(10 5 4 16 9 8)','qq - advanced list->vector'))



### or
tests.append(('or',))
tests.append(("(or)", '#f', 'or with no params'))
tests.append(("(or 2 3 4)", '2', 'OR with no #f args'))
tests.append(("(or #f 3 4)", '3', 'OR with first arg as #f'))
tests.append(("(or #f #f)", '#f', 'OR with all args as #f'))
tests.append(("(or #t #f)", '#t', 'OR with first arg as #t'))
tests.append(("(or (or) (or) 5 (or))", '5', "nested OR"))

### and
tests.append(('and',))
tests.append(("(and)", '#t', 'and with no params'))
tests.append(("(and 1)", '1', 'and with one integer param'))
tests.append(("(and 1 #f)", '#f', 'and with #f'))

### null
tests.append(('null?',))
tests.append(("(null? '())", '#t', 'NULL? on empty static list'))
tests.append(("(null? (list))", '#t', 'NULL? on empty dynamic list'))
tests.append(("(null? '(2))", '#f', 'NULL? on NON empty static list'))
tests.append(("(null? (list 3))", '#f', 'NULL? on NON empty dynamic list'))

### number?
tests.append(('number?',))
tests.append(("(number? 5)", '#t', 'NUMBER? on integer constant'))
tests.append(("(number? ((lambda () 6)))", '#t', 'NUMBER? on result of a lambda applic (int)'))
tests.append(("(number? #t)", '#f', 'NUMBER? on boolean'))
tests.append(("(number? '())", '#f', 'NUMBER? on nil'))

### string-ref
tests.append(('string-ref',))
tests.append(("(string-ref \"Hello World!\" 0)", '#\\H', 'string-ref first character'))
tests.append(("(string-ref \"Hello World!\" 6)", '#\\W', 'string-ref middle character'))

### string-length
tests.append(('string-length',))
tests.append(("(string-length \"Hello World!\")", '12', 'string-length on a string'))
tests.append(("(string-length \"\")", '0', 'string-length on an empty string'))

### zero?
tests.append(('zero?',))
tests.append(("(zero? 0)", '#t', 'ZERO? on a 0 constant'))
tests.append(("(zero? 5)", '#f', 'ZERO? on a 5 constant'))
tests.append(("(zero? 2/4)", '#f', 'ZERO? on a 2/4 constant'))
tests.append(("(zero? 0/4)", '#t', 'ZERO? on a 0/4 constant'))

### char?
tests.append(('char?',))
tests.append(("(char? #\\b)", '#t', 'CHAR? on a char constant'))
tests.append(("(char? \"hi\")", '#f', 'CHAR? on a string constant'))
tests.append(("(char? 5)", '#f', 'CHAR? on a 5 constant'))

### string?
tests.append(('string?',))
tests.append(("(string? \"some string\")", '#t', 'STRING? on a string constant'))
tests.append(("(string? 'hi)", '#f', 'STRING? on a symbol'))

### procedure?
tests.append(('procedure?',))
tests.append(("(procedure? (lambda (x) 6))", '#t', 'PROCEDURE? on lambda simple'))
tests.append(("(procedure? (lambda (x . y) 6))", '#t', 'PROCEDURE? on lambda opt'))
tests.append(("(procedure? (lambda x 6))", '#t', 'PROCEDURE? on lambda variadic'))
tests.append(("(procedure? car)", '#t', 'PROCEDURE? on a global procedure'))
tests.append(("(procedure? ((lambda x 6) 8))", '#f', 'PROCEDURE? on applic'))

### pair?
tests.append(('pair?',))
tests.append(("(pair? '(2 . 3))", '#t', 'PAIR? on a constant pair'))
tests.append(("(pair? (cons 2 3))", '#t', 'PAIR? on a dynamic pair'))
tests.append(("(pair? '(5))", '#t', 'PAIR? on a constant list with one element'))
tests.append(("(pair? '(5 6))", '#t', 'PAIR? on a constant list with two elements'))
tests.append(("(pair? '())", '#f', 'PAIR? on the empty list'))
tests.append(("(pair? 8)", '#f', 'PAIR? on an integer constant'))

### boolean?
tests.append(('boolean?',))
tests.append(("(boolean? #t)", '#t', 'BOOLEAN? on constant #t'))
tests.append(("(boolean? #f)", '#t', 'BOOLEAN? on constant #f'))
tests.append(("(boolean? 7)", '#f', 'BOOLEAN? on an integer constant'))

### car
tests.append(('car',))
tests.append(("(car '(3 5))", '3', 'CAR on a constant list'))
tests.append(("(car (list \"hello\" \"world\"))", '"hello"', 'CAR on a dynamic list'))

### cdr
tests.append(('cdr',))
tests.append(("(cdr '(3 5))", '(5 . ())', 'CDR on a constant list'))
tests.append(("(cdr (list \"hello\" \"world\"))", '("world" . ())', 'CDR on a dynamic list'))

### cons
tests.append(('cons',))
tests.append(("(cons 1 2)", '(1 . 2)', 'CONS on integer constants'))
tests.append(("(cons 1 (cons 2 '()))", '(1 . (2 . ()))', 'CONS recursive to create a list'))
tests.append(("(eq? (cdr (cons 1 '(1 2 3))) '(1 2 3))", '#t', 'cons should not make a copy of args'))

### remainder
tests.append(('remainder',))
tests.append(("(remainder 8 10)", '8', 'remainder for x > y (positive)'))
tests.append(("(remainder -8 10)", '-8', 'remainder for x > y (negative)'))
tests.append(("(remainder 8 3)", '2', 'remainder for x < y (positive)'))
tests.append(("(remainder -8 3)", '-2', 'remainder for x < y (negative)'))

### larger than
tests.append(('>',))
tests.append(("(> 3 2 1)", '#t', '> for x > y > z (integers)'))
tests.append(("(> 3 2 2)", '#f', '> for x > y = z (integers)'))
tests.append(("(> 5/2 4/3 -1/2)", '#t', '> for x > y > z (fractions)'))
tests.append(("(> 5/2 5/2 4/3)", '#f', '> for x = y > z (fractions)'))
tests.append(("(> 5 4/3 -3 -20/3 -50)", '#t', '> for x > y > z (mixed)'))
tests.append(("(> 250 3 3)", '#f', '> for x > y = z (mixed)'))
tests.append(("(> 0/4 3)", '#f', '< for fraction and integer , fraction has 0 as nominator'))

### smaller than
tests.append(('<',))
tests.append(("(< 1 2 3)", '#t', '< for x < y < z (integers)'))
tests.append(("(< 1 2 2)", '#f', '< for x < y = z (integers)'))
tests.append(("(< -1/2 4/3 5/2)", '#t', '< for x < y < z (fractions)'))
tests.append(("(< 4/3 5/2 5/2)", '#f', '< for x = y < z (fractions)'))
tests.append(("(< -50 -20/3 -3 4/3 5)", '#t', '< for x < y < z (mixed)'))
tests.append(("(< 3 3 250)", '#f', '< for x < y = z (mixed)'))
tests.append(("(< 0/4 3)", '#t', '< for fraction and integer , fraction has 0 as nominator'))
tests.append(('=',))
tests.append(("(= 2 2)", '#t', '= for 2 constant args (integers)'))
tests.append(("(= 0 0/3)", '#t', '= for 2 constant fraction and integer, fraction has 0 as nominator'))
tests.append(("(= 2/4 2/4)", '#t', '= for 2 constant args (fractions)'))
tests.append(("(= 4/2 2)", '#t', '= for 2 constant args (mixed)'))
tests.append(("(= 2 4/2)", '#t', '= for 2 constant args (mixed) (reverse of previous)'))
tests.append(("(= 4/2 2 8/4 16/8 2)", '#t', '= for 5 constant args (mixed)'))
tests.append(("(= 4/2 2 8/4 16/8 2 0)", '#f', '= for 6 constant args (mixed)'))

### plus
tests.append(('+',))
tests.append(("(+)", '0', '+ for no args'))
tests.append(("(+ 2)", '2', '+ for 1 constant args (integers)'))
tests.append(("(+ 2 10)", '12', '+ for 2 constant args (integers)'))
tests.append(("(+ 2 -10)", '-8', '+ for 2 constant args (integers)'))
tests.append(("(+ 4/16)", ['1/4','4/16'], '+ for 1 constant args (fractions) (with minimization)'))
tests.append(("(+ 1/2 1/4)", ['3/4','6/8'], '+ for 2 constant args (fractions)'))
tests.append(("(+ 1/2 -1/4)", ['1/4','2/8'], '+ for 2 constant args (fractions)'))
tests.append(("(+ 1/2 -2 3/2 5 10)", ['15', '15/1','60/4'] , '+ for 5 constant args (mixed)'))
tests.append(("(= 2/3 (+ 1/3 1/3))", '#t', '= and + on fractions'))
tests.append(('*',))
tests.append(("(*)", '1', '* for no args'))
tests.append(("(* 2)", '2', '* for 1 constant args (integers)'))
tests.append(("(* 2 10)", '20', '* for 2 constant args (integers)'))
tests.append(("(* 2 -10)", '-20', '* for 2 constant args (integers)'))
tests.append(("(* 4/16)", ['1/4','4/16'], '* for 1 constant args (fractions) (with minimization)'))
tests.append(("(* 1/2 1/4)", '1/8', '* for 2 constant args (fractions)'))
tests.append(("(* 1/2 -1/4)", '-1/8', '* for 2 constant args (fractions)'))
tests.append(("(* 1/2 -2 3/2 5 10)", ['-75' , '-75/1','-300/4'], '* for 5 constant args (mixed)'))

### divide
tests.append(('/',))
tests.append(("(/ 3)",'1/3', '/ with 1 integer'))
tests.append(("(/ 1/4)",['4','4/1'], '/ with 1 fraction'))
tests.append(("(/ -3)",'-1/3', '/ with 1 integer,negative'))
tests.append(("(/ -2/5)",'-5/2', '/ with 1 fraction,negative'))
tests.append(("(/ 3 4)",'3/4', '/ with 2 integers'))
tests.append(("(/ -3 4)",'-3/4', '/ with 2 integers, 1 negative'))
tests.append(("(/ 3 -4)",'-3/4', '/ with 2 integers, 1 negative'))
tests.append(("(/ -3 5 7)",'-3/35', '/ with 3 integers, 1 negative'))
tests.append(("(/ 3 -5 7)",'-3/35', '/ with 3 integers, 1 negative'))
tests.append(("(/ 3 5 -7)",'-3/35', '/ with 3 integers, 1 negative'))
tests.append(("(/ 2/7 3)",'2/21', '/ with 1 integer, 1 fraction'))
tests.append(("(/ 3 2/7)",'21/2', '/ with 1 integer, 1 fraction'))
tests.append(("(/ -2/7 3)",'-2/21', '/ with 1 integer, 1 fraction, 1 negative'))
tests.append(("(/ 5/7 3/2)",'10/21', '/ with 2 fractions'))
tests.append(("(/ -5/7 3/2)",'-10/21', '/ with 2 fractions, 1 negative'))
tests.append(("(/ 3/5 7 11)",'3/385', '/ with 1 fraction, 2 integers'))
tests.append(("(/ 3/5 7 11/23)",'69/385', '/ with 2 fractions, 1 integer'))
tests.append(("(/ -3/5 7 11/23)",'-69/385', '/ with 2 fractions, 1 integer, 1 negative'))
tests.append(("(/ 3/5 -7 11/23)",'-69/385', '/ with 2 fractions, 1 integer, 1 negative'))
tests.append(("(/ 3/5 7 -11/23)",'-69/385', '/ with 2 fractions, 1 integer, 1 negative'))
tests.append(("(/ -3/5 -7 11/23)",'69/385', '/ with 2 fractions, 1 integer, 2 negatives'))
tests.append(("(/ 3/5 -7 -11/23)",'69/385', '/ with 2 fractions, 1 integer, 2 negatives'))
tests.append(("(/ -3/5 7 -11/23)",'69/385', '/ with 2 fractions, 1 integer, 2 negatives'))

### char->integer
tests.append(('char->integer',))
tests.append((r"(char->integer #\a)", '97', 'get ascii value of a'))

### integer->char
tests.append(('integer->char',))
tests.append(("(integer->char 97)", r"#\a", 'get char of ascii value 97'))

### symbols
tests.append(('symbols',))
tests.append(("'hi", 'hi', 'return a symbol (in uppercase!)'))
tests.append(("(string->symbol \"hi\")", 'hi', 'return a symbol from a string representation'))
tests.append(("(symbol->string 'hi)", '"hi"', 'return a string from a symbol representation'))
tests.append(("(eq? (string->symbol \"hi\") 'hi)", '#f', 'eq between dynamic retrieval of a symbol from string and the symbol'))
tests.append(("(eq? (symbol->string 'hi) \"hi\")", '#f', 'eq between dynamic retrieval of a string from symbol and the string'))
tests.append(("(eq? (string->symbol \"HI\") 'hi)", '#t', 'eq between dynamic retrieval of a symbol from string and the symbol'))
tests.append(("(eq? (symbol->string 'hi) \"HI\")", '#t', 'eq between dynamic retrieval of a string from symbol and the string'))
tests.append(("(eq? (string->symbol (symbol->string 'hi)) 'hi)", '#t', 'symbol->string and string->symbol invariant test'))
tests.append(("(eq? 'hi \"hi\")", '#f', 'a symbol and the matching string are different objects'))
tests.append(("(eq? (string->symbol \"sss\") (string->symbol (make-string 3 #\s)))", "#t", 'eq of two different dynamic symbols'))

### string
tests.append(('string',))
tests.append((r"(make-string 0 #\a)", '""', 'create an empty string'))
tests.append((r"(make-string 4 #\a)", '"aaaa"', 'create a repetitive string'))
tests.append(("(string-length \"aaaa\")", '4', 'string-length of "aaaa"'))
tests.append(("(string-length \"\")", '0', 'string-length of ""'))
tests.append(("(string-ref \"abcd\" 0)", r"#\a", 'string-ref 0 on "abcd"'))
tests.append(("(string-ref \"\n\" 0)", r"#\newline", r"string-ref 0 on \"\\n\""))

### vector
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

### list
tests.append(('list',))
tests.append(("(list)", '()', 'create an empty list'))
tests.append(("(eq? '() (list))", '#t', 'empty list should be a singleton'))
tests.append(("(list 1)", '(1 . ())', 'create a list with one element'))
tests.append(("(list 1 2 3 4 5 6)", '(1 . (2 . (3 . (4 . (5 . (6 . ()))))))', 'create a list with multiple elements'))

### eq?
tests.append(('eq? implementation',))
tests.append(("(eq? '(7 8) '(7 8))", '#t', 'same constant lists should be eq'))
tests.append(("(eq? '() (cdr '(5)))", '#t', 'cdr of a one-element list should be nil'))
tests.append(("(eq? 0 0)", '#t', 'eq of two integers'))
tests.append(("(eq? #f #f)", '#t', 'eq of two booleans'))
tests.append(("(eq? 1/2 1/2)", '#t', 'eq of two fractions'))
tests.append(("(eq? 68/55 (+ 3/5 7/11))", '#t', 'eq of two fractions'))
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

### letrec
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
(define <= (lambda x (or (apply < x) (apply = x))))
(define pivot (lambda (l)
  (cond ((null? l) 'done)
        ((null? (cdr l)) 'done)
        ((<= (car l) (car (cdr l))) (pivot (cdr l)))
        (else (car l))
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
               (quicksort (car (cdr parts))))))))

(quicksort '(3 8 5 4 8 2 4 1 9 4))
""", '(1 . (2 . (3 . (4 . (4 . (4 . (5 . (8 . (8 . (9 . ()))))))))))', 'quicksort'))


tests.append(('additional by kobi tests',))

tests.append(("""
'(a (+ 1 2) c)
`(a (+ 1 2) c)
`(a ,(+ 1 2) c)
(define fact 
        (lambda (n)
                (if (zero? n)
                1
                `(* ,n ,(fact (- n 1))))))
(fact 4)
((lambda (x) `(,x ',x)) '(lambda (x) `(,x',x)))
`','moshe
(define (reverse1 lis)
        (if (null? lis)
        '()
        (append (reverse1 (cdr lis)) (list (car lis)))))
`(a ,(reverse1 '(x y z)) b)
`(a ,@(reverse1 '(x y z)) b)
(define a 1) 
(define b 2)
(define c '(x y z))
(define d '(u v w))
'(a b c)
`(,a b ,c)
`(q r ,a ,b s t ,@c ,d)
`(,@2)
""", """(A . ((+ . (1 . (2 . ()))) . (C . ())))
(A . ((+ . (1 . (2 . ()))) . (C . ())))
(A . (3 . (C . ())))
(* . (4 . ((* . (3 . ((* . (2 . ((* . (1 . (1 . ()))) . ()))) . ()))) . ())))
((LAMBDA . ((X . ()) . ((QUASIQUOTE . (((UNQUOTE . (X . ())) . ((QUOTE . ((UNQUOTE . (X . ())) . ())) . ())) . ())) . ()))) . ((QUOTE . ((LAMBDA . ((X . ()) . ((QUASIQUOTE . (((UNQUOTE . (X . ())) . ((QUOTE . ((UNQUOTE . (X . ())) . ())) . ())) . ())) . ()))) . ())) . ()))
(QUOTE . (MOSHE . ()))
(A . ((Z . (Y . (X . ()))) . (B . ())))
(A . (Z . (Y . (X . (B . ())))))
(A . (B . (C . ())))
(1 . (B . ((X . (Y . (Z . ()))) . ())))
(Q . (R . (1 . (2 . (S . (T . (X . (Y . (Z . ((U . (V . (W . ()))) . ()))))))))))
2""", 'quasi-quoting'))


tests.append(("""
(apply (lambda (x) ((lambda (x) x) 5)) 3 '())
(apply (lambda (x) ((lambda () x) )) 3 '())
(apply cons 1 2 '())
(apply cons '(1 2))
(apply cons 1 '(2))
(apply (lambda (x y) (apply cons x y '())) '(1 2))
(apply + '(4 5))
(apply vector 'a 'b '(c d e)) 
(define first
  (lambda (l)
    (apply (lambda (x . y) x)
             l)))
(define rest
  (lambda (l)
    (apply (lambda (x . y) y) l)))
(first '(a b c d))
(rest '(a b c d)) 
(apply (lambda (x .y) y) '(1))
(apply (lambda x x) '())
""", """5
3
(1 . 2)
(1 . 2)
(1 . 2)
(1 . 2)
9
#5(A B C D E)
A
(B . (C . (D . ())))
()
()""", 'apply'))

tests.append(("""
(let ((this 'this) (is 'is) (silly 'silly)) (let ((f$ (lambda (f n k) (if (zero? n) (k 1) (f f (- n 1) (lambda (r) (k (* n r)))))))) `(,this ,is ,silly ,(f$ f$ 10 (lambda (x) x)))))
""", """(THIS . (IS . (SILLY . (3628800 . ()))))""", 'this is silly'))


tests.append(("""
(append '(401) '(402))        
(append '(400 401) '(402 403))   
(cons (append '(1) '(2)) '(1))        
(eq? (append '(1) '(2)) '(1))     
(append '(a b c) '()) 
(append '() '(a b c)) 
(append '(a b) '(c d)) 
(append '(a b) 'c) 
(let ((x (list 'b)))
  (eq? x (cdr (append '(a) x))))
(eq? 'A (string->symbol "A"))
;#t
(eq? 'a (string->symbol "A"))    
;#t
(define lis (list 1 2 3))
lis
(define lis (cdr lis))
lis

(string->symbol "abc")
;abc
(eq? (string->symbol "A") 'a)
;#t
(eq? (string->symbol "A") 'A)
;#t
(eq? 'a 'A)
;#t
(symbol->string 'xyz)
;"XYZ"
(map + '(1 2) '(1 2) '(1 2))
;(3 6)
(map (lambda (x) (* x x))
     '(1 -3 -5 7))
(map cons '(a b c) '(1 2 3))
((lambda (x) (x '(1 2))) car)
""", """(401 . (402 . ()))
(400 . (401 . (402 . (403 . ()))))
((1 . (2 . ())) . (1 . ()))
#f
(A . (B . (C . ())))
(A . (B . (C . ())))
(A . (B . (C . (D . ()))))
(A . (B . C))
#t
#t
#t
(1 . (2 . (3 . ())))
(2 . (3 . ()))
abc
#t
#t
#t
"XYZ"
(3 . (6 . ()))
(1 . (9 . (25 . (49 . ()))))
((A . 1) . ((B . 2) . ((C . 3) . ())))
1""", 'mix1'))

tests.append(("""
;line command
'(1 2 #;3 4)
'(((((((((1)) 2) 3) 4) 5) 6) 7 ) 8)
'((1 . 2) ((3 . 4) 5))
(define a #f)
(if a 5)
(define a 5)
(cond ((null? 7) 7))
(define (lvar . x) x)
(define (lopt . (a . c)) c)
(lvar)
(lvar '())
(lvar 1)
(lvar 1 2 3 4 5)
(lopt 1)
(lopt 1 2 3 4 5)
(lopt '(1 2 3) '(4 5 6) 7)
(define lvar 7/55)
(or #f (pair? '()) (boolean? 22) (char? "i am char!!!") (integer? '(1 2 3)) (null? '(1)) (procedure? lvar) (string? #\L) (symbol? "LVAR") (vector? #\K) 666)
(and (or #f #f #t) (and 1 3 4) (or (or) (and)) #f)
(define (a . (x)) x)
(define b #f)
(if (a #t) (or b b b 1 2) 666)
((lambda (a b c) ((lambda (a b) a) 666 2)) 3 4 5)
((lambda (a b . c) a) 1 2)
((lambda (a b . c) b) 1 2)
((lambda (a b . c) c) 1 2)
((lambda (a b . c) a) 1 2 3 4 5)
((lambda (a b . c) b) 1 2 3 4 5)
((lambda (a b . c) c) 1 2 3 4 5)
((lambda (a . c) c) 1 2 3 4 5 6 7 8 9 10)

(let* ((x 1)
       (y (+ x 1))
       (z (lambda (x) (* x y))))
    (list y x (z y)))

(define prog  (lambda (a b c)
        ((lambda (d e) 
                ((lambda (f) 
                        ((lambda (g h) 
                                ((lambda (e) a)5)
                        )
                        7 8)
                )
                6)
         )
         4 5)
)
)

(prog 1 2 3)
(prog 4 5 6)
(define a 3)
(prog a 6 7)
(define prog 7)
prog
""", """(1 . (2 . (4 . ())))
(((((((((1 . ()) . ()) . (2 . ())) . (3 . ())) . (4 . ())) . (5 . ())) . (6 . ())) . (7 . ())) . (8 . ()))
((1 . 2) . (((3 . 4) . (5 . ())) . ()))
()
(() . ())
(1 . ())
(1 . (2 . (3 . (4 . (5 . ())))))
()
(2 . (3 . (4 . (5 . ()))))
((4 . (5 . (6 . ()))) . (7 . ()))
666
#f
1
666
1
2
()
1
2
(3 . (4 . (5 . ())))
(2 . (3 . (4 . (5 . (6 . (7 . (8 . (9 . (10 . ())))))))))
(2 . (1 . (4 . ())))
1
4
3
7""", 'mix2'))

tests.append(("""
(define accumulate 
        (lambda (op initial sequence) 
                (if (null? sequence) 
                        initial 
                        (op (car sequence) (accumulate op initial (cdr sequence))))))

(define ls  '((1 2 3) (4 6 7) (8 9)))

(accumulate append (list) (map (lambda (x) x) ls))

(accumulate append (list) (map cdr ls))

(define flatmap(lambda (proc seq) 
 (accumulate append (list) (map proc seq)))) 

(define (filter1 pred? lst)
  (cond ((null? lst) (list ))
        ((pred? (car lst)) (cons (car lst) (filter1 pred? (cdr lst))))
        (else (filter1 pred? (cdr lst)))))

 (flatmap (lambda (lst) (filter1 (lambda(x)  (= (remainder x 2) 1))  lst))  ls) 
 
 (define id (lambda (x) x))
 
 (define fib$ 
        (lambda (n c) 
                (cond ((= n 0) (c 0)) 
                      ((= n 1) (c 1)) 
                      (else (fib$ (- n 1) (lambda (fib-n-1) 
                                                (fib$ (- n 2) (lambda (fib-n-2) 
                                                                        (c (+ fib-n-1 fib-n-2)))))))))) 
 (fib$ 7 id)
 
 (define not1 
        (lambda (x)
                (if x 
                    #f
                    #t)))
                    
(define mul-list$ 
        (lambda (ls succ fail) 
                (cond ((null? ls) (succ 1)) 
                      ((not1 (pair? ls)) (if (number? ls) (succ ls) (fail 'not-a-number!))) 
                      (else (mul-list$ (car ls) 
                                       (lambda (mul-car) 
                                               (mul-list$ (cdr ls) 
                                                           (lambda (mul-cdr) 
                                                            (succ (* mul-car mul-cdr))) 
                                                fail)) 
                                         fail))))) 

(mul-list$ (list 1 2 (list 3 4 5) (list 6 7 10)) (lambda (x) x) (lambda (x) x)) 

(mul-list$ '(1 2 (3 4 5) (6 7 'a)) (lambda (x) x) (lambda (x) x))
""", """(1 . (2 . (3 . (4 . (6 . (7 . (8 . (9 . ()))))))))
(2 . (3 . (6 . (7 . (9 . ())))))
(1 . (3 . (7 . (9 . ()))))
13
50400
NOT-A-NUMBER!""", 'ppl'))

tests.append(("""
(eq? 'a 'a)
(eq? "kobi" "kobi")
(eq? 1/2 2/4)
(eq? 5 5)
(eq? '(a b c) (cdr '(a a b c)))
(eq? '#('a) '#('a))
(eq? #f #f)
(eq? '() '())
(eq? (if #f 1) (if #f 2))
(eq? 'a 'ab)
(eq? "kobi" "kkobi")
(eq? 1/2 2/3)
(eq? 5 5/1)
(eq? 5 6)
(eq? '(a b c) (cdr '(a b b c)))
(eq? '#('a) '#('a 1))
(eq? #f #t)
(eq? '() '(a))
(eq? (if #t 1) (if #f 2))

(string-length "abqwedfghg")
(string-length "")
(vector-length '#(1 2 3 4 5 6 7 8 9 'a))
(vector-length '#())
(char->integer #\h)
(char->integer (integer->char 101))

(define x 5)
(string->symbol "x")
x
'x
(string->symbol "KObI")
'KObI
(eq? (string->symbol "a") 'a)
(eq? (string->symbol "X") 'x)
(eq? (string->symbol "kk") (string->symbol "kk"))

(remainder 16 4)
(remainder 5 2)
(remainder -45 7)
(remainder 10 -3)
(remainder -17 -9)

(vector-ref '#(a b c) 0)
(vector-ref '#(a b c) 1)
(vector-ref '#(x y z w) 3)
(vector-ref '#(x y 77 w) 2)

(make-vector 0)
(make-vector 0 'a)
(make-vector 5 'a)
(make-vector 5 1)
(make-vector 5)

(= 1)
(= 1/2 1/2)
(= 2 4/2 8/4 16/8 32/16)
(= 1/2 2/4 3/6 6/12)
(= 1 2)
(= 1 1/2)
(= 1/2 1/4)
(= 2 4/2 7/2 16/8 32/16)
(= 1/2 2/3 3/6 6/12)

(> 1/2)
(> 4 3/2 1 1/2)
(> 1 1/2 1/3 1/4 1/5 1/6 1/7)
(> 4 5)
(> 1 1/2 1/3 1/2 1/3)
(> 1/3 1/4 1/4 1/5)
(> 5 6/1)

(< 1/2)
(< 5 6 8/1 100/2)
(< 3/2 3)
(< 1 1/2 1/3 1/4 1/5 1/6 1/7)
(< 14 5)
(< 1 2 3 4 5 6 6/2)
(< 1/3 1/4 1/4 1/5)
""","""#t
#t
#t
#t
#t
#t
#t
#t
#t
#f
#f
#f
#f
#f
#f
#f
#f
#f
#f
10
0
10
0
104
101
x
5
X
KObI
KOBI
#f
#t
#t
0
1
-3
1
-8
A
B
W
77
#0()
#0()
#5(A A A A A)
#5(1 1 1 1 1)
#5(0 0 0 0 0)
#t
#t
#t
#t
#f
#f
#f
#f
#f
#t
#t
#t
#f
#f
#f
#f
#t
#t
#t
#f
#f
#f
#f""", 'func'))

tests.append(('Offical tests',))
tests.append(("""
#t
#f
'()
""", """#t
#f
()""", 'file 1'))
tests.append(("""
(if #f #f #t)
(if #t #t #f)
""", """#t
#t""", 'file 2'))
tests.append(("""
(or)
(or #t)
(or #f)
(or #f #t)
(or #f #f #f #f #f #t #f)
(and)
(and #t)
(and #t #t)
(and #t #f)
(and #f #t)
(and #t #t #t #t #t #t #t #f)
""", """#f
#t
#f
#t
#t
#t
#t
#t
#f
#f
#f""", 'file 3'))
tests.append((r"""
1
2
3
#\a
#\A
#\newline
#\"
#\\
3/4
4/5
6/7
'(1 2 3)
'(1 . (2 . (3 . ())))
'((1 2) (3 4))
'1234
'#\a
""", r"""1
2
3
#\a
#\A
#\newline
#\"
#\\
3/4
4/5
6/7
(1 . (2 . (3 . ())))
(1 . (2 . (3 . ())))
((1 . (2 . ())) . ((3 . (4 . ())) . ()))
1234
#\a""", 'file 4'))
tests.append(("""
(let ((x #f))
  (let ()
    x))

(let ((x #f) (y #t))
  (let ((x #f))
    (let ((x #f) (z #f) (t #f))
      (let ((x #f) (t #f))
  y))))

((((lambda (x)
     (lambda (y)
       y))
   (lambda (p)
     (p (lambda (x y)
    (lambda (p)
      (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))

((((lambda (x)
     (lambda (y)
       (x y)))
   (lambda (p)
     (p (lambda (x y)
    (lambda (p)
      (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))

((((lambda (x)
     (lambda (y)
       (x (x y))))
   (lambda (p)
     (p (lambda (x y)
    (lambda (p)
      (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))

(((((lambda (x) ((x x) (x x)))
    (lambda (x)
      (lambda (y)
  (x (x y)))))
   (lambda (p)
     (p (lambda (x y)
    (lambda (p)
      (p y x))))))
  (lambda (z) (z #t #f)))
 (lambda (x y) x))
""", """#f
#t
#t
#f
#t
#t""", 'file 5'))
tests.append(("""
(let ()
  ((lambda s
     (let ()
       ((lambda s s) s s s)))
   #t))
""", "((#t . ()) . ((#t . ()) . ((#t . ()) . ())))", 'file 6'))
tests.append(("""
(define test
  (let ((p1 (lambda (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
	      (lambda (z)
		(z x2 x3 x4 x5 x6 x7 x8 x9 x10 x1))))
	(s '(a b c d e f g h i j)))
    (lambda ()
      (((((((((((apply p1 s) p1) p1) p1) p1) p1) p1) p1) p1) p1)
	       list))))

(test)
""", "(A . (B . (C . (D . (E . (F . (G . (H . (I . (J . ()))))))))))", 'file 7'))
tests.append(("""
(((((lambda (x) (x (x x)))
    (lambda (x)
      (lambda (y)
  (x (x y)))))
   (lambda (p)
     (p (lambda (x)
    (lambda (y)
      (lambda (z)
        ((z y) x)))))))
  (lambda (x)
    ((x #t) #f)))
 (lambda (x)
   (lambda (y)
     x)))
""", "#t", 'file 8'))
tests.append((r"""
(boolean? #t)
(boolean? #f)
(boolean? 1234)
(boolean? 'a)
(symbol? 'b)
(procedure? procedure?)
(eq? (car '(a b c)) 'a)
(= (car (cons 1 2)) 1)
(integer? 1234)
(char? #\a)
(null? '())
(string? "abc")
(symbol? 'lambda)
(vector? '#(1 2 3))
(vector? 1234)
(string? '#(a b c))
(string? 1234)
(= 3 (vector-length '#(a #t ())))
(pair? '(a . b))
(pair? '())
(zero? 0)
(zero? 234)
(+ 2 2/3)
(* 1/3 3/5 5/7)
(+ 1/2 1/3)
""", """#t
#t
#f
#f
#t
#t
#t
#t
#t
#t
#t
#t
#t
#t
#f
#f
#f
#t
#t
#f
#t
#f
8/3
1/7
5/6""", 'file 9'))
tests.append(("""
(define with (lambda (s f) (apply f s)))

(define crazy-ack
  (letrec ((ack3
      (lambda (a b c)
        (cond
         ((and (zero? a) (zero? b)) (+ c 1))
         ((and (zero? a) (zero? c)) (ack-x 0 (- b 1) 1))
         ((zero? a) (ack-z 0 (- b 1) (ack-y 0 b (- c 1))))
         ((and (zero? b) (zero? c)) (ack-x (- a 1) 1 0))
         ((zero? b) (ack-z (- a 1) 1 (ack-y a 0 (- c 1))))
         ((zero? c) (ack-x (- a 1) b (ack-y a (- b 1) 1)))
         (else (ack-z (- a 1) b (ack-y a (- b 1) (ack-x a b (- c 1))))))))
     (ack-x
      (lambda (a . bcs)
        (with bcs
    (lambda (b c)
      (ack3 a b c)))))
     (ack-y
      (lambda (a b . cs)
        (with cs
    (lambda (c)
      (ack3 a b c)))))
     (ack-z
      (lambda abcs
        (with abcs
    (lambda (a b c)
      (ack3 a b c))))))
    (lambda ()
      (and (= 7 (ack3 0 2 2))
           (= 61 (ack3 0 3 3))
           (= 316 (ack3 1 1 5))
           (= 636 (ack3 2 0 1))
     ))))

(crazy-ack)
""", "#t", 'file 10'))
tests.append(("""
(define fact
  (lambda (n)
    (if (zero? n)
      1
      (* n (fact (- n 1)))
    )
  )
)

(fact 5)
""", "120", 'file 11'))
tests.append(("""
(letrec ((fact-1
    (lambda (n)
      (if (zero? n)
    1
    (* n (fact-2 (- n 1))))))
   (fact-2
    (lambda (n)
      (if (zero? n)
    1
    (* n (fact-3 (- n 1))))))
   (fact-3
    (lambda (n)
      (if (zero? n)
    1
    (* n (fact-4 (- n 1))))))
   (fact-4
    (lambda (n)
      (if (zero? n)
    1
    (* n (fact-5 (- n 1))))))
   (fact-5
    (lambda (n)
      (if (zero? n)
    1
    (* n (fact-1 (- n 1)))))))
  (fact-1 10))
""", "3628800", 'file 12'))


if __name__ == "__main__":
	main(tests)
