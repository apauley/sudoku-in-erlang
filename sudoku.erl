-module(sudoku).
-compile(export_all).

test() ->
    ["A1","A2","B1","B2"] = cross("AB", "12"),
    81 = length(squares()),
    [["A1","B1","C1","D1","E1","F1","G1","H1","I1"]|_] = col_squares(),
    [["A1","A2","A3","A4","A5","A6","A7","A8","A9"]|_] = row_squares(),
    ok.

cross(SeqA, SeqB) ->
    %% Cross product of elements in SeqA and elements in SeqB.
    [[X,Y] || X <- SeqA, Y <- SeqB].

digits() ->
    "123456789".
rows() ->
    "ABCDEFGHI".
cols() ->
    digits().

squares() ->
    cross(rows(), cols()).

col_squares() ->
    [cross(rows(), [C]) || C <- cols()].
row_squares() ->
    [cross([R], cols()) || R <- rows()].
