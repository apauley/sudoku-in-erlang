-module(unittests).
-import(lists, [all/2, member/2]).
-import(sudoku, [cross/2,
                 squares/0, col_squares/0, row_squares/0, box_squares/0,
                 unitlist/0, units/1, peers/1,
                 grid_values/1, parse_grid/1, eliminate/3, assign/3,
                 display/1]).
-export([test/0]).

%% Test the functionality using Joe Armstrong's Micro Lightweight Unit Testing:
%% http://armstrongonsoftware.blogspot.com/2009/01/micro-lightweight-unit-testing.html
test() ->
    {ok, cross} = test_cross(),
    {ok, squares} = test_squares(),
    {ok, unitlist} = test_unitlist(),
    {ok, units} = test_units(),
    {ok, peers} = test_peers(),
    {ok, grid_values} = test_grid_values(),
    {ok, parse_grid} = test_parse_grid(),
    {ok, eliminate} = test_eliminate(),
    {ok, assign} = test_assign(),
    {ok, assign_elimination} = test_assign_eliminates_from_peers(),
    {ok, recursive_peer_elimination} = test_recursive_peer_elimination(),
    {ok, display} = test_display(),
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

test_grid_values() ->
    GridString = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......",
    ValuesDict = grid_values(GridString),
    81 = length(dict:fetch_keys(ValuesDict)),
    "4" = dict:fetch("A1", ValuesDict),
    "123456789" = dict:fetch("A2", ValuesDict),
    "8" = dict:fetch("A7", ValuesDict),
    "3" = dict:fetch("B2", ValuesDict),
    {ok, grid_values}.

test_parse_grid() ->
    GridString = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......",

    %% Unparsed values for reference
    ValuesDict = grid_values(GridString),
    "123456789" = dict:fetch("F2", ValuesDict),

    %% A parsed grid will already  have determined the value of some squares
    ParsedDict = parse_grid(GridString),
    "456789" = dict:fetch("F2", ParsedDict),
    {ok, parse_grid}.

test_eliminate() ->
    GridString = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......",
    ValuesDict = eliminate(grid_values(GridString), ["A2"], "3"),
    "12456789" = dict:fetch("A2", ValuesDict),
    "2457" = dict:fetch("A2", eliminate(ValuesDict, ["A2"], "13689")),
    {ok, eliminate}.

test_assign() ->
    GridString = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......",
    ValuesDict = assign(grid_values(GridString), "A2", $1),
    "1" = dict:fetch("A2", ValuesDict),
    {ok, assign}.

test_assign_eliminates_from_peers() ->
    GridString = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......",
    NonPeerValues = dict:fetch("D1", grid_values(GridString)),
    ValuesDict = assign(grid_values(GridString), "A3", $7),

    %% Now 7 may not be a possible value in any of A3's peers
    Fun = fun(Square) -> not (member($7, dict:fetch(Square, ValuesDict))) end,
    true = all(Fun, peers("A3")),

    %% After assignment, the non-peers remain unchanged:
    NonPeerValues = dict:fetch("D1", ValuesDict),
    {ok, assign_elimination}.

test_recursive_peer_elimination() ->
    GridString = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......",
    %% Eliminate all but two values from a peer of A3:
    SetupDict = eliminate(grid_values(GridString), ["A2"], "2345689"),
    "17" = dict:fetch("A2", SetupDict),

    %% Assigning one of the above two values in A3 should trigger
    %% peer elimination in A2 as well.
    ValuesDict = assign(SetupDict, "A3", $7),
    "1" = dict:fetch("A2", ValuesDict),
    Fun = fun(Square) -> not (member($1, dict:fetch(Square, ValuesDict))) end,
    true = all(Fun, peers("A2")),

    {ok, recursive_peer_elimination}.

test_display() ->
    GridString = ".17369825632158947958724316825437169791586432346912758289643571573291684164875293",
    [$4|T] = display(eliminate(grid_values(GridString), ["A1"], "12356789")),
    [$.|T] = GridString,
    {ok, display}.

allTrue(Booleans) ->
    %% Test support function:
    %% Returns true if the list of booleans are all true.
    %% I expect there should already be such a function,
    %% please point me to it if you can.
    all(fun(Bool) -> Bool end, Booleans).
