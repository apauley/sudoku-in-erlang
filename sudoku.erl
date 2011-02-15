-module(sudoku).
-import(lists, [member/2]).
-compile(export_all).

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
    %% Returns a list of 81 square names, including "A1" etc.
    cross(rows(), cols()).

col_squares() ->
    %% All the square names for each column.
    [cross(rows(), [C]) || C <- cols()].
row_squares() ->
    %% All the square names for each row.
    [cross([R], cols()) || R <- rows()].
box_squares() ->
    %% All the square names for each box.
    [cross(Rows, Cols) || Rows <- ["ABC", "DEF", "GHI"],
                          Cols <- ["123", "456", "789"]].

unitlist() ->
    %% A list of all units (columns, rows, boxes) in a grid.
    col_squares() ++ row_squares() ++ box_squares().

units(Square) ->
    %% A list of units for a specific square
    [S || S <- unitlist(), member(Square, S)].

peers(Square) ->
    %% A unique list of squares (excluding this one)
    %% that are also part of the units for this square.
    NonUniquePeers = shallow_flatten([S || S <- units(Square)]),
    PeerSet = sets:from_list(NonUniquePeers),
    PeersWithSelf = sets:to_list(PeerSet),
    lists:delete(Square, PeersWithSelf).

shallow_flatten([]) -> [];
shallow_flatten(List) ->
    [H|T] = List,
    H ++ shallow_flatten(T).

grid_values(GridString) ->
    %% Converts a string of values into a dictionary of values keyed on square name.
    %% Non-digits and "0" is allowed here, it indicates an unset square.
    81 = length(GridString),
    Tuples =  lists:zipwith(fun zipfun/2, squares(), GridString),
    dict:from_list(Tuples).

zipfun(Square, Digit) ->
    Digits = digits(),
    case member(Digit, Digits) of
	true -> {Square, [Digit]};
	false -> {Square, Digits}
    end.

eliminate(ValuesDict, Square, DropDigits) ->
    %% Eliminate all specified values for this square
    OldValues = dict:fetch(Square, ValuesDict),
    NewValues = lists:filter(fun(Elem) -> not member(Elem, DropDigits) end, OldValues),
    dict:store(Square, NewValues, ValuesDict).

assign(ValuesDict, Square, Digit) ->
    %% assign should be implemented as the elimination of all values except for the assigned value.
    %% For now, just assign the value directly.
    dict:store(Square, [Digit], ValuesDict).
