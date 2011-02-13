-module(sudoku).
-compile(export_all).

test() ->
    [] = cross('AB', '12'),
    ok.

cross(A, B) ->
    [].
