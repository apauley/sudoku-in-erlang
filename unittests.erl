-module(unittests).
-import(lists, [all/2, member/2]).
-import(sudoku, [cross/2, digits/0,
                 squares/0, col_squares/0, row_squares/0, box_squares/0,
                 unitlist/0, units/1, peers/1,
                 clean_grid/1, is_solved/1, time_solve/1,
                 empty_dict/0, parse_grid/1, eliminate/3, assign/3,
                 places_for_value/3, to_string/1]).
-export([test/0]).

%% Test the functionality using Joe Armstrong's Micro Lightweight Unit Testing:
%% http://armstrongonsoftware.blogspot.com/2009/01/micro-lightweight-unit-testing.html
test() ->
    ok = test_cross(),
    ok = test_squares(),
    ok = test_unitlist(),
    ok = test_units(),
    ok = test_peers(),
    ok = test_empty_dict(),
    ok = test_clean_grid(),
    ok = test_parse_grid(),
    ok = test_eliminate(),
    ok = test_assign(),
    ok = test_assign_eliminates_from_peers(),
    ok = test_recursive_peer_elimination(),
    ok = test_automatically_assign_unique_places(),
    ok = test_places_for_value(),
    ok = test_is_solved(),
    ok = test_time_solve(),
    ok = test_to_string(),
    ok.

test_cross() ->
    ["A1","A2","B1","B2"] = cross("AB", "12"),
    ok.

test_squares() ->
    81 = length(squares()),
    ok.

test_unitlist() ->
    [["A1","B1","C1","D1","E1","F1","G1","H1","I1"]|_] = col_squares(),
    [["A1","A2","A3","A4","A5","A6","A7","A8","A9"]|_] = row_squares(),
    [["A1","A2","A3","B1","B2","B3","C1","C2","C3"]|_] = box_squares(),
    27 = length(unitlist()),
    ok.

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
    ok.

test_peers() ->
    Peers = lists:sort(["C8", "F2", "G2", "H2", "C7",
                       "I2", "A3", "A1", "C9", "A2",
                       "B1", "B2", "B3", "C3", "C1",
                       "C4", "D2", "C6", "C5", "E2"]),
    Peers = lists:sort(peers("C2")),

    %% Each square should have exactly 20 squares as its peers
    true = all(fun(Units) -> length(Units) == 20 end,
               [peers(Square) || Square <- squares()]),
    ok.

test_empty_dict() ->
    ValuesDict = empty_dict(),
    true = is_sudoku_dict(ValuesDict),

    %% The values of all keys should start with all possible values.
    Squares = squares(),
    Digits = digits(),
    true = all(fun(Values) -> Values == Digits end,
               [dict:fetch(Square, ValuesDict) || Square <- Squares]),
    ok.

test_clean_grid() ->
    GridString = "|4..-...-805|
.3.+...+...",
    "4.....805.3......." = clean_grid(GridString),
    ok.

test_parse_grid() ->
    GridString = "4.....8.5
.3.......
...7.....
.2.....6.
....8.4..
....1....
...6.3.7.
5..2.....
1.4......",

    %% A parsed grid will already have eliminated the values of some squares
    ParsedDict = parse_grid(GridString),
    "4" = dict:fetch("F2", ParsedDict),
    ok.

test_eliminate() ->
    ValuesDict = eliminate(empty_dict(), ["A2"], "3"),
    "12456789" = dict:fetch("A2", ValuesDict),
    "2457" = dict:fetch("A2", eliminate(ValuesDict, ["A2"], "13689")),
    ok.

test_assign() ->
    ValuesDict = assign(empty_dict(), "A2", $1),
    "1" = dict:fetch("A2", ValuesDict),
    ok.

test_assign_eliminates_from_peers() ->
    NonPeerValues = dict:fetch("D1", empty_dict()),
    ValuesDict = assign(empty_dict(), "A3", $7),

    %% Now 7 may not be a possible value in any of A3's peers
    Fun = fun(Square) -> not (member($7, dict:fetch(Square, ValuesDict))) end,
    true = all(Fun, peers("A3")),

    %% After assignment, the non-peers remain unchanged:
    NonPeerValues = dict:fetch("D1", ValuesDict),
    ok.

test_recursive_peer_elimination() ->
    %% Eliminate all but two values from a peer of A3:
    SetupDict = eliminate(empty_dict(), ["A2"], "2345689"),
    "17" = dict:fetch("A2", SetupDict),

    %% Assigning one of the above two values in A3 should trigger
    %% peer elimination in A2 as well.
    ValuesDict = assign(SetupDict, "A3", $7),
    "1" = dict:fetch("A2", ValuesDict),
    Fun = fun(Square) -> not (member($1, dict:fetch(Square, ValuesDict))) end,
    true = all(Fun, peers("A2")),
    ok.

test_automatically_assign_unique_places() ->
    %% This grid was chosen so that C9 is a unique place for the digit 2
    GridString = ".....3.17.15..9..8.6.......1....7.....9...2.....5....4.......2.5..6..34.34.2.....",
    ValuesDict = parse_grid(GridString),
    "2" = dict:fetch("C9", ValuesDict),
    ok.

test_places_for_value() ->
    GridString = ".45.81376........................................................................",
    ValuesDict = parse_grid(GridString),
    "29" = dict:fetch("A1", ValuesDict),
    "29" = dict:fetch("A4", ValuesDict),
    Unit = ["A1","A2","A3","A4","A5","A6","A7","A8","A9"],
    ["A1","A4"] = places_for_value(ValuesDict, Unit, $9),
    ["A1","A4"] = places_for_value(ValuesDict, Unit, $2),
    ok.

test_is_solved() ->
    GridString = "417369825632158947958724316825437169791586432346912758289643571573291684164875293",
    SolvedDict = parse_grid(GridString),
    true = is_solved(SolvedDict),
    false = is_solved(empty_dict()),
    ok.

test_time_solve() ->
    GridString = ".45.81376........................................................................",
    {MicroSeconds, Dict} = time_solve(GridString),
    true = is_integer(MicroSeconds),
    true = is_sudoku_dict(Dict),
    ok.

test_to_string() ->
    GridString = ".17369825632158947958724316825437169791586432346912758289643571573291684164875293",
    [$4|T] = to_string(eliminate(parse_grid(GridString), ["A1"], "12356789")),
    [$.|T] = GridString,
    ok.

is_sudoku_dict(ValuesDict) ->
    lists:sort(dict:fetch_keys(ValuesDict)) == squares().

allTrue(Booleans) ->
    %% Test support function:
    %% Returns true if the list of booleans are all true.
    %% I expect there should already be such a function,
    %% please point me to it if you can.
    all(fun(Bool) -> Bool end, Booleans).
