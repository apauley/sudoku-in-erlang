-module(sudoku).
-compile(export_all).

test() ->
    ["A1", "A2", "B1", "B2"] = cross("AB", "12"),
    ok.

cross(SeqA, SeqB) ->
    %% Cross product of elements in SeqA and elements in SeqB.
    [[X,Y] || X <- SeqA, Y <- SeqB].
