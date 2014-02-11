compilation-compiler-tester
===========================
Usage
-----
Copy _tester.py_ to project folder and execute
```
python3 tester.py [-h] [-c CODE] {all,skip,only} 
```
Optional arguments:
  -h, --help            Show help message and exit
  -c CODE, --code CODE  Compile code directly
  -l, --list            Show a list of available sections and thier numbers

subcommands:
  valid subcommands

  {all,skip,only}       additional help
    all                 Run all tests (default)
    skip                Skip sections of tests or specific tests
    only                Run specific sections of tests or specific tests

  Both *skip* and *only* expect one of the following flags:
    -s NUM              Number of section to skip/execute
    -t num              Number of test to skip/execute