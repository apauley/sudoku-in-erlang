-module(unittests).
-import(lists, [all/2, member/2, sort/1]).
-import(sudoku, [cross/2, values/2,
                 squares/0, col_squares/0, row_squares/0, box_squares/0,
                 unitlist/0, units/1, peers/1, search/1,
                 least_valued_unassigned_square/1, stats/1,
                 clean_grid/1, is_solved/1, eliminate_digits/3,
                 empty_puzzle/0, parse_grid/1, eliminate/3, assign/3,
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
    ok = test_empty_puzzle(),
    ok = test_clean_grid(),
    ok = test_parse_grid(),
    ok = test_least_valued_unassigned_square(),
    ok = test_eliminate(),
    ok = test_search_bails_out_early(),
    ok = test_search_solves_grid(),
    ok = test_assign(),
    ok = test_assign_eliminates_from_peers(),
    ok = test_recursive_peer_elimination(),
    ok = test_automatically_assign_unique_places(),
    ok = test_places_for_value(),
    ok = test_is_solved(),
    ok = test_to_string(),
    ok = test_stats(),
    ok.

test_cross() ->
    ["a1","a2","b1","b2"] = cross("ab", "12"),
    ok.

test_squares() ->
    81 = length(squares()),
    ok.

test_unitlist() ->
    [["a1","b1","c1","d1","e1","f1","g1","h1","i1"]|_] = col_squares(),
    [["a1","a2","a3","a4","a5","a6","a7","a8","a9"]|_] = row_squares(),
    [["a1","a2","a3","b1","b2","b3","c1","c2","c3"]|_] = box_squares(),
    27 = length(unitlist()),
    ok.

test_units() ->
    [["a2","b2","c2","d2","e2","f2","g2","h2","i2"]|_] = units("c2"),

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
    Peers = sort(["c8", "f2", "g2", "h2", "c7",
                  "i2", "a3", "a1", "c9", "a2",
                  "b1", "b2", "b3", "c3", "c1",
                  "c4", "d2", "c6", "c5", "e2"]),
    Peers = sort(peers("c2")),

    %% Each square should have exactly 20 squares as its peers
    true = all(fun(Units) -> length(Units) == 20 end,
               [peers(Square) || Square <- squares()]),
    ok.

test_empty_puzzle() ->
    Puzzle = empty_puzzle(),
    {_, Eliminations} = Puzzle,
    0 = Eliminations,
    true = is_sudoku_puzzle(Puzzle),

    %% The values of all keys should start with all possible values.
    Squares = squares(),
    Digits = "123456789",
    true = all(fun(Values) -> Values == Digits end,
               [values(Puzzle, Square) || Square <- Squares]),
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
    Puzzle = parse_grid(GridString),
    "4" = values(Puzzle, "f2"),
    ok.

test_least_valued_unassigned_square() ->
    %% Assign something to A1 and eliminate another from A2.
    %% A1 should not be considered, it's already assigned.
    Puzzle = assign(eliminate_digits(empty_puzzle(), "a2", "234"), "a1", $1),
    false = is_solved(Puzzle),
    {"a2", _} = least_valued_unassigned_square(Puzzle),

    %% Any square can be returned when all values are equally unassigned
    {"a1", _} = least_valued_unassigned_square(empty_puzzle()),
    ok.

test_eliminate() ->
    Puzzle = eliminate(empty_puzzle(), ["a2"], $3),
    "12456789" = values(Puzzle, "a2"),
    NewPuzzle = eliminate_digits(Puzzle, "a2", "13689"),
    "2457" = values(NewPuzzle, "a2"),

    %% Eliminating the last value from a square should indicate an error
    {false, _} = eliminate_digits(Puzzle, "a2", "123456789"),
    ok.

test_search_bails_out_early() ->
    %% Searching an already solved puzzle should just return it unharmed.
    true = solved_puzzle() =:= search(solved_puzzle()),

    %% Searching a previous failure should return the same failure
    {false, 1} = search({false, 1}),
    ok.

test_search_solves_grid() ->
    GridString = "4.....8.5.3..........7......2...
..6.....8.4......1.......6.3.7.5..2.....1.4......",
    ValuesTuple = parse_grid(GridString),
    false = is_solved(ValuesTuple),
    true = is_solved(search(ValuesTuple)),
    ok.

test_assign() ->
    Puzzle = assign(empty_puzzle(), "a2", $1),
    "1" = values(Puzzle, "a2"),

    %% Assigning a different value to an already assigned square should
    %% indicate an error.
    {false, _} = assign(Puzzle, "a2", $3),
    ok.

test_assign_eliminates_from_peers() ->
    NonPeerValues = values(empty_puzzle(), "d1"),
    Puzzle = assign(empty_puzzle(), "a3", $7),

    %% Now 7 may not be a possible value in any of a3's peers
    Fun = fun(Square) -> not (member($7, values(Puzzle, Square))) end,
    true = all(Fun, peers("a3")),

    %% After assignment, the non-peers remain unchanged:
    NonPeerValues = values(Puzzle, "d1"),
    ok.

test_recursive_peer_elimination() ->
    %% Eliminate all but two values from a peer of a3:
    SetupPuzzle = eliminate_digits(empty_puzzle(), "a2", "2345689"),
    "17" = values(SetupPuzzle, "a2"),

    %% Assigning one of the above two values in a3 should trigger
    %% peer elimination in a2 as well.
    Puzzle = assign(SetupPuzzle, "a3", $7),
    "1" = values(Puzzle, "a2"),
    Fun = fun(Square) -> not (member($1, values(Puzzle, Square))) end,
    true = all(Fun, peers("a2")),
    ok.

test_automatically_assign_unique_places() ->
    %% This grid was chosen so that C9 is a unique place for the digit 2
    GridString = ".....3.17.15..9..8.6.......1....
7.....9...2.....5....4.......2.5..6..34.34.2.....",
    Puzzle = parse_grid(GridString),
    "2" = values(Puzzle, "c9"),
    ok.

test_places_for_value() ->
    GridString = ".45.81376.......................
.................................................",
    Puzzle = parse_grid(GridString),
    "29" = values(Puzzle, "a1"),
    "29" = values(Puzzle, "a4"),
    Unit = ["a1","a2","a3","a4","a5","a6","a7","a8","a9"],
    ["a1","a4"] = places_for_value(Puzzle, Unit, $9),
    ["a1","a4"] = places_for_value(Puzzle, Unit, $2),
    ok.

test_is_solved() ->
    true = is_solved(solved_puzzle()),
    false = is_solved(empty_puzzle()),
    ok.

test_to_string() ->
    GridString = ".1736982563215894795872431682543
7169791586432346912758289643571573291684164875293",
    Puzzle = eliminate_digits(parse_grid(GridString), "a1", "12356789"),
    [$4|T] = to_string(Puzzle),
    [$.|T] = clean_grid(GridString),
    ok.

test_stats() ->
    {Total, Avg, Med, Max, Min, Length} = stats([2, 9, 4]),
    15 = Total,
    5.0 = Avg,
    4 = Med,
    9 = Max,
    2 = Min,
    3 = Length,
    ok.

is_sudoku_puzzle(Puzzle) ->
    {ValuesDict, Eliminations} = Puzzle,
    true = is_integer(Eliminations),
    is_sudoku_dict(ValuesDict).

is_sudoku_dict(ValuesDict) ->
    lists:sort(dict:fetch_keys(ValuesDict)) == squares().

solved_puzzle() ->
    GridString = "41736982563215894795872431682543
7169791586432346912758289643571573291684164875293",
    parse_grid(GridString).

allTrue(Booleans) ->
    %% Test support function:
    %% Returns true if the list of booleans are all true.
    %% I expect there should already be such a function,
    %% please point me to it if you can.
    all(fun(Bool) -> Bool end, Booleans).
