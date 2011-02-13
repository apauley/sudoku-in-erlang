-module(sudoku).
-import(lists, [all/2, member/2]).
-compile(export_all).

%% Test the functionality using Joe Armstrong's Micro Lightweight Unit Testing:
%% http://armstrongonsoftware.blogspot.com/2009/01/micro-lightweight-unit-testing.html
test() ->
    {ok, cross} = test_cross(),
    {ok, squares} = test_squares(),
    {ok, unitlist} = test_unitlist(),
    {ok, units} = test_units(),
    {ok, peers} = test_peers(),
    {ok, sudoku}.

test_cross() ->
    ["A1","A2","B1","B2"] = cross("AB", "12"),
    {ok, cross}.

test_squares() ->
    81 = length(squares()),
    {ok, squares}.

test_unitlist() ->
    [["A1","B1","C1","D1","E1","F1","G1","H1","I1"]|_] = col_squares(),
    [["A1","A2","A3","A4","A5","A6","A7","A8","A9"]|_] = row_squares(),
    [["A1","A2","A3","B1","B2","B3","C1","C2","C3"]|_] = box_squares(),
    27 = length(unitlist()),
    {ok, unitlist}.

test_units() ->
    [["A2","B2","C2","D2","E2","F2","G2","H2","I2"]|_] = units("C2"),

    %% Each square should have exactly 3 units
    true = all(fun(Units) -> length(Units) == 3 end,
               [units(Square) || Square <- squares()]),

    %% Each unit should contain exactly nine squares
    TruthValues = [all(fun(Unit) -> length(Unit) == 9 end,
                       units(Square)) || Square <- squares()],

    %% Each square should be part of all its units
    TruthValues = [all(fun(Unit) -> member(Square, Unit) end,
                       units(Square)) || Square <- squares()],

    true = allTrue(TruthValues),
    {ok, units}.

test_peers() ->
    Peers = lists:sort(["C8", "F2", "G2", "H2", "C7",
                       "I2", "A3", "A1", "C9", "A2",
                       "B1", "B2", "B3", "C3", "C1",
                       "C4", "D2", "C6", "C5", "E2"]),
    Peers = lists:sort(peers("C2")),

    %% Each square should have exactly 20 squares as its peers
    true = all(fun(Units) -> length(Units) == 20 end,
               [peers(Square) || Square <- squares()]),
    {ok, peers}.

allTrue(Booleans) ->
    %% Test support function:
    %% Returns true if the list of booleans are all true.
    %% I expect there should already be such a function,
    %% please point me to it if you can.
    all(fun(Bool) -> Bool end, Booleans).

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
