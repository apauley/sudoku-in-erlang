-module(unittests).
-import(lists, [all/2, member/2, sort/1]).
-import(sudoku, [cross/2, digits/0, values/2,
                 squares/0, col_squares/0, row_squares/0, box_squares/0,
                 unitlist/0, units/1, peers/1, search/1,
                 least_valued_unassigned_square/1,
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
    Peers = sort(["C8", "F2", "G2", "H2", "C7",
                       "I2", "A3", "A1", "C9", "A2",
                       "B1", "B2", "B3", "C3", "C1",
                       "C4", "D2", "C6", "C5", "E2"]),
    Peers = sort(peers("C2")),

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
    Digits = digits(),
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
    "4" = values(Puzzle, "F2"),
    ok.

test_least_valued_unassigned_square() ->
    %% Assign something to A1 and eliminate another from A2.
    %% A1 should not be considered, it's already assigned.
    Puzzle = assign(eliminate_digits(empty_puzzle(), "A2", "234"), "A1", $1),
    false = is_solved(Puzzle),
    {"A2", _} = least_valued_unassigned_square(Puzzle),

    %% Any square can be returned when all values are equally unassigned
    {"A1", _} = least_valued_unassigned_square(empty_puzzle()),
    ok.

test_eliminate() ->
    Puzzle = eliminate(empty_puzzle(), ["A2"], $3),
    "12456789" = values(Puzzle, "A2"),
    NewPuzzle = eliminate_digits(Puzzle, "A2", "13689"),
    "2457" = values(NewPuzzle, "A2"),

    %% Eliminating the last value from a square should indicate an error
    {false, _} = eliminate_digits(Puzzle, "A2", digits()),
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
    Puzzle = assign(empty_puzzle(), "A2", $1),
    "1" = values(Puzzle, "A2"),

    %% Assigning a different value to an already assigned square should
    %% indicate an error.
    {false, _} = assign(Puzzle, "A2", $3),
    ok.

test_assign_eliminates_from_peers() ->
    NonPeerValues = values(empty_puzzle(), "D1"),
    Puzzle = assign(empty_puzzle(), "A3", $7),

    %% Now 7 may not be a possible value in any of A3's peers
    Fun = fun(Square) -> not (member($7, values(Puzzle, Square))) end,
    true = all(Fun, peers("A3")),

    %% After assignment, the non-peers remain unchanged:
    NonPeerValues = values(Puzzle, "D1"),
    ok.

test_recursive_peer_elimination() ->
    %% Eliminate all but two values from a peer of A3:
    SetupPuzzle = eliminate_digits(empty_puzzle(), "A2", "2345689"),
    "17" = values(SetupPuzzle, "A2"),

    %% Assigning one of the above two values in A3 should trigger
    %% peer elimination in A2 as well.
    Puzzle = assign(SetupPuzzle, "A3", $7),
    "1" = values(Puzzle, "A2"),
    Fun = fun(Square) -> not (member($1, values(Puzzle, Square))) end,
    true = all(Fun, peers("A2")),
    ok.

test_automatically_assign_unique_places() ->
    %% This grid was chosen so that C9 is a unique place for the digit 2
    GridString = ".....3.17.15..9..8.6.......1....
7.....9...2.....5....4.......2.5..6..34.34.2.....",
    Puzzle = parse_grid(GridString),
    "2" = values(Puzzle, "C9"),
    ok.

test_places_for_value() ->
    GridString = ".45.81376.......................
.................................................",
    Puzzle = parse_grid(GridString),
    "29" = values(Puzzle, "A1"),
    "29" = values(Puzzle, "A4"),
    Unit = ["A1","A2","A3","A4","A5","A6","A7","A8","A9"],
    ["A1","A4"] = places_for_value(Puzzle, Unit, $9),
    ["A1","A4"] = places_for_value(Puzzle, Unit, $2),
    ok.

test_is_solved() ->
    true = is_solved(solved_puzzle()),
    false = is_solved(empty_puzzle()),
    ok.

test_to_string() ->
    GridString = ".1736982563215894795872431682543
7169791586432346912758289643571573291684164875293",
    Puzzle = eliminate_digits(parse_grid(GridString), "A1", "12356789"),
    [$4|T] = to_string(Puzzle),
    [$.|T] = clean_grid(GridString),
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
