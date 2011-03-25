This is an Erlang version of a sudoku solver, mostly based on the
solution by Peter Norvig.
For comparison, see my slightly modified version of Norvig's Python code:
https://github.com/apauley/sudoku-by-norvig

To try it out, simply run the sudoku script:
$ ./sudoku

You will need to have Erlang installed with escript in your path.

To only run the tests:
$ ./sudoku runtests

To solve the sample files without running tests:
$ ./sudoku solve
