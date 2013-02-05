-module(sudoku).

-export([solve_all/1,
         solve_file/2,
         print_results/1]).

-export([test/0]).

-compile({parse_transform, ct_expand}).

-define(digits, "123456789").
-define(rows, "abcdefghi").
-define(cols, ?digits).

solve_all(GridList) ->
  SolutionDicts = solve_all_return_dicts(GridList),
  [to_string(S) || S <- SolutionDicts].

print_results(Filename) ->
  print_results(Filename, "\n").

print_results(Filename, Seperator) ->
  {Time, Solutions} = timer:tc(sudoku, solve_file,
                               [Filename, Seperator]),
  Solved = [Puzzle
            || Puzzle <- Solutions, is_solved(Puzzle)],
  TimeInSeconds = Time / 1000000,
  Eliminations = [Count || {_, Count} <- Solutions],
  {Total, Avg, Med, Max, Min, NumberPuzzles} =
    stats(Eliminations),
  Hz = NumberPuzzles / TimeInSeconds,
  Msg = "Solved ~p of ~p puzzles from ~s in ~f "
    "secs (~.2f Hz)\n  (~p total eliminations, "
    "avg ~.2f, median ~p, max ~p, min ~p).~n",
  io:format(Msg,
            [length(Solved), NumberPuzzles, Filename, TimeInSeconds,
             Hz, Total, Avg, Med, Max, Min]).

stats(List) ->
  Total = lists:sum(List),
  Length = length(List),
  Avg = Total / Length,
  Med = lists:nth(round(Length / 2), lists:sort(List)),
  Max = lists:max(List),
  Min = lists:min(List),
  {Total, Avg, Med, Max, Min, Length}.

squares() ->
  %% Returns a list of 81 square names, including 'a1' etc.
  ct_expand:term([list_to_atom([X, Y])
                  || X <- ?rows, Y <- ?cols]).

unitlist() ->
  %% A list of all units (columns, rows, boxes) in a grid.
  ct_expand:term([[list_to_atom([X, Y])
                   || X <- ?rows, Y <- [C]]
                  || C <- ?cols]
                 ++
                   [[list_to_atom([X, Y]) || X <- [R], Y <- ?cols]
                    || R <- ?rows]
                 ++
                   [[list_to_atom([X, Y]) || X <- R, Y <- C]
                    || R <- ["abc", "def", "ghi"],
                       C <- ["123", "456", "789"]]).

units(Square) ->
  %% A list of units for a specific square
  [S || S <- unitlist(), lists:member(Square, S)].

peers(Square) ->
  %% A unique list of squares (excluding this one)
  %% that are also part of the units for this square.
  NonUniquePeers = lists:flatten([S || S <- units(Square)]),
  lists:delete(Square, lists:usort(NonUniquePeers)).

values(Puzzle, Square) ->
  %% Returns the digit values for a given square
  {Dict, _} = Puzzle,
  dict:fetch(Square, Dict).

parse_grid(GridString) ->
  CleanGrid = clean_grid(GridString),
  81 = length(CleanGrid),
  parse_puzzle(empty_puzzle(), squares(), CleanGrid).

clean_grid(GridString) ->
  %% Return a string with only digits, 0 and .
  ValidChars = (?digits) ++ "0.",
  [E || E <- GridString, lists:member(E, ValidChars)].

parse_puzzle(Puzzle, [], []) -> Puzzle;
parse_puzzle(Puzzle, [Square | Squares],
             [Value | GridString]) ->
  IsDigit = lists:member(Value, ?digits),
  NewPuzzle = assign_if_digit(Puzzle, Square, Value,
                              IsDigit),
  parse_puzzle(NewPuzzle, Squares, GridString).

assign_if_digit(Puzzle, Square, Value, true) ->
  %% Value is a Digit, possible to assign
  assign(Puzzle, Square, Value);
assign_if_digit(Puzzle, _, _, false) ->
  %% Not possible to assign
  Puzzle.

empty_puzzle() -> {empty_dict(), 0}.

empty_dict() ->
  dict:from_list([{Square, ?digits}
                  || Square <- squares()]).

assign(Puzzle, Square, Digit) ->
  %% Assign by eliminating all values except the assigned value.
  OtherValues = exclude_from(values(Puzzle, Square),
                             Digit),
  eliminate_digits(Puzzle, Square, OtherValues).

eliminate_digits({false, _Count} = False, _, _) ->
  False;
eliminate_digits(Puzzle, _, []) -> Puzzle;
eliminate_digits(Puzzle, Square, [Digit | T]) ->
  PuzzleOrFalse = eliminate(Puzzle, [Square], Digit),
  eliminate_digits(PuzzleOrFalse, Square, T).

eliminate({false, _Count} = False, _, _) -> False;
eliminate(Puzzle, [], _) -> Puzzle;
eliminate(Puzzle, [Square | T], Digit) ->
  %% Eliminate the specified Digit from all specified Squares.
  OldValues = values(Puzzle, Square),
  NewValues = exclude_from(OldValues, Digit),
  NewPuzzle = eliminate(Puzzle, Square, Digit, NewValues,
                        OldValues),
  eliminate(NewPuzzle, T, Digit).

eliminate({_, Count}, _, _, [], _) ->
  %% Contradiction: removed last value
  {false, Count};
eliminate(Puzzle, _, _, Vs, Vs) ->
  %% NewValues and OldValues are the same, already eliminated.
  Puzzle;
eliminate({ValuesDict, Eliminations}, Square, Digit,
          NewValues, _) ->
  NewDict = dict:store(Square, NewValues, ValuesDict),
  NewPuzzle = peer_eliminate({NewDict, Eliminations + 1},
                             Square, NewValues),
  %% Digit have been eliminated from this Square.
  %% Now see if the elimination has created a unique place for a digit
  %% to live in the surrounding units of this Square.
  assign_unique_place(NewPuzzle, units(Square), Digit).

peer_eliminate(Puzzle, Square, [AssignedValue]) ->
  %% If there is only one value left, we can also
  %% eliminate that value from the peers of Square
  eliminate(Puzzle, peers(Square), AssignedValue);
peer_eliminate(Puzzle, _, _) ->
  %% Multiple values, cannot eliminate from peers.
  Puzzle.

assign_unique_place({false, _Count} = False, _, _) ->
  False;
assign_unique_place(Puzzle, [], _) -> Puzzle;
assign_unique_place(Puzzle, [Unit | T], Digit) ->
  %% If a certain digit can only be in one place in a unit,
  %% assign it.
  Places = places_for_value(Puzzle, Unit, Digit),
  NewPuzzle = assign_unique_place_for_digit(Puzzle,
                                            Places, Digit),
  assign_unique_place(NewPuzzle, T, Digit).

assign_unique_place_for_digit({_, Count}, [], _) ->
  %% Contradiction: no place for Digit found
  {false, Count};
assign_unique_place_for_digit(Puzzle, [Square],
                              Digit) ->
  %% Unique place for Digit found, assign
  assign(Puzzle, Square, Digit);
assign_unique_place_for_digit(Puzzle, _, _) ->
  %% Mutlitple palces (or none) found for Digit
  Puzzle.

places_for_value(Puzzle, Unit, Digit) ->
  [Square
   || Square <- Unit,
      lists:member(Digit, values(Puzzle, Square))].

solve(GridString) -> search(parse_grid(GridString)).

search({false, _Count} = False) -> False;
search(Puzzle) -> search(Puzzle, is_solved(Puzzle)).

search(Puzzle, true) ->
  %% Searching an already solved puzzle should just return it unharmed.
  Puzzle;
search(Puzzle, false) ->
  {Square, Values} =
    least_valued_unassigned_square(Puzzle),
  first_valid_result(Puzzle, Square, Values).

%% Returns the first valid puzzle, otherwise the last puzzle
first_valid_result({_, Count}, _, []) -> {false, Count};
first_valid_result(Puzzle, Square,
                   [Digit | _T] = Digits) ->
  PuzzleOrFalse = search(assign(Puzzle, Square, Digit)),
  first_valid_result(Puzzle, Square, Digits,
                     PuzzleOrFalse).

first_valid_result({Dict, ValidCount}, Square, [_ | T],
                   {false, InvalidCount}) ->
  first_valid_result({Dict,
                      ValidCount + (InvalidCount - ValidCount)},
                     Square, T);
first_valid_result(_, _, _, Puzzle) -> Puzzle.

least_valued_unassigned_square({ValuesDict, _}) ->
  Lengths = [values_length(V) || V <- dict:to_list(ValuesDict)],
  Unassigned = [L || L <- Lengths, unassigned(L)],
  {_, Square, Values} = lists:min(Unassigned),
  {Square, Values}.

unassigned({Length, _, _}) -> Length > 1.

values_length({S, Values}) ->
  {length(Values), S, Values}.

solve_all_return_dicts(GridList) ->
  PidGrids = [{spawn(fun server/0), Grid}
              || Grid <- GridList],
  lists:foreach(fun send_puzzle/1, PidGrids),
  [receive_solution(V) || V <- PidGrids].

send_puzzle({Pid, Grid}) -> Pid ! {self(), solve, Grid}.

receive_solution({Pid, Grid}) ->
  receive {Pid, Grid, Solution} -> Solution end.

server() ->
  receive
    {From, solve, GridString} ->
      From ! {self(), GridString, solve(GridString)}
  end.

is_solved(Puzzle) ->
  lists:all(fun (Unit) ->
                is_unit_solved(Puzzle, Unit)
            end,
            unitlist()).

is_unit_solved(Puzzle, Unit) ->
  UnitValues = lists:flatmap(fun (S) -> values(Puzzle, S)
                             end,
                             Unit),
  lists:sort(UnitValues) =:= (?digits).

to_string(Puzzle) ->
  {ValuesDict, _} = Puzzle,
  Fun = fun ({_, [V]}) -> [V];
            ({_, _}) -> "."
        end,
  lists:flatmap(Fun,
                lists:sort(dict:to_list(ValuesDict))).

from_file(Filename, Seperator) ->
  {ok, BinData} = file:read_file(Filename),
  string:tokens(binary_to_list(BinData), Seperator).

to_file(Filename, Solutions) ->
  GridStrings = [to_string(S) ++ "\n" || S <- Solutions],
  ok = file:write_file(Filename,
                       list_to_binary(GridStrings)).

solve_file(Filename, Seperator) ->
  Solutions = solve_all_return_dicts(from_file(Filename, Seperator)),
  OutFilename = [filename:basename(Filename, ".txt")
                 | ".out"],
  ok = to_file(OutFilename, Solutions),
  Solutions.

exclude_from(Values, Digit) ->
  lists:delete(Digit, Values).

%%%====================================================================================
%%% Micro Unit Tests
%%% ----------------
%%%
%%% Test the functionality using Joe Armstrong's Micro Lightweight Unit Testing:
%%% http://armstrongonsoftware.blogspot.com/2009/01/micro-lightweight-unit-testing.html
%%%
%%% This was moved into the sudoku module in order to get rid of export_all
%%%====================================================================================

test() ->
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

test_squares() ->
  81 = length(squares()),
  ok.

test_unitlist() ->
  27 = length(unitlist()),
  ok.

test_units() ->
  [[a2,b2,c2,d2,e2,f2,g2,h2,i2]|_] = units(c2),

  %% Each square should have exactly 3 units
  true = lists:all(fun(Units) -> length(Units) =:= 3 end,
                   [units(Square) || Square <- squares()]),

  %% Each unit should contain exactly nine squares
  TruthValues = [lists:all(fun(Unit) -> length(Unit) =:= 9 end,
                           units(Square)) || Square <- squares()],

  %% Each square should be part of all its units
  TruthValues = [lists:all(fun(Unit) -> lists:member(Square, Unit) end,
                           units(Square)) || Square <- squares()],

  true = allTrue(TruthValues),
  ok.

test_peers() ->
  Peers = lists:sort([c8, f2, g2, h2, c7,
                      i2, a3, a1, c9, a2,
                      b1, b2, b3, c3, c1,
                      c4, d2, c6, c5, e2]),
  Peers = lists:sort(peers(c2)),

  %% Each square should have exactly 20 squares as its peers
  true = lists:all(fun(Units) -> length(Units) =:= 20 end,
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
  true = lists:all(fun(Values) -> Values =:= Digits end,
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
"4" = values(Puzzle, f2),
ok.

test_least_valued_unassigned_square() ->
  %% Assign something to A1 and eliminate another from A2.
  %% A1 should not be considered, it's already assigned.
  Puzzle = assign(eliminate_digits(empty_puzzle(), a2, "234"), a1, $1),
  false = is_solved(Puzzle),
  {a2, _} = least_valued_unassigned_square(Puzzle),

  %% Any square can be returned when all values are equally unassigned
  {a1, _} = least_valued_unassigned_square(empty_puzzle()),
  ok.

test_eliminate() ->
  Puzzle = eliminate(empty_puzzle(), [a2], $3),
  "12456789" = values(Puzzle, a2),
  NewPuzzle = eliminate_digits(Puzzle, a2, "13689"),
  "2457" = values(NewPuzzle, a2),

  %% Eliminating the last value from a square should indicate an error
  {false, _} = eliminate_digits(Puzzle, a2, "123456789"),
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
  Puzzle = assign(empty_puzzle(), a2, $1),
  "1" = values(Puzzle, a2),

  %% Assigning a different value to an already assigned square should
  %% indicate an error.
  {false, _} = assign(Puzzle, a2, $3),
  ok.

test_assign_eliminates_from_peers() ->
  NonPeerValues = values(empty_puzzle(), d1),
  Puzzle = assign(empty_puzzle(), a3, $7),

  %% Now 7 may not be a possible value in any of a3's peers
  Fun = fun(Square) -> not (lists:member($7, values(Puzzle, Square))) end,
  true = lists:all(Fun, peers(a3)),

  %% After assignment, the non-peers remain unchanged:
  NonPeerValues = values(Puzzle, d1),
  ok.

test_recursive_peer_elimination() ->
  %% Eliminate all but two values from a peer of a3:
  SetupPuzzle = eliminate_digits(empty_puzzle(), a2, "2345689"),
  "17" = values(SetupPuzzle, a2),

  %% Assigning one of the above two values in a3 should trigger
  %% peer elimination in a2 as well.
  Puzzle = assign(SetupPuzzle, a3, $7),
  "1" = values(Puzzle, a2),
  Fun = fun(Square) -> not (lists:member($1, values(Puzzle, Square))) end,
  true = lists:all(Fun, peers(a2)),
  ok.

test_automatically_assign_unique_places() ->
  %% This grid was chosen so that C9 is a unique place for the digit 2
  GridString = ".....3.17.15..9..8.6.......1....
7.....9...2.....5....4.......2.5..6..34.34.2.....",
    Puzzle = parse_grid(GridString),
  "2" = values(Puzzle, c9),
  ok.

test_places_for_value() ->
  GridString = ".45.81376.......................
.................................................",
    Puzzle = parse_grid(GridString),
  "29" = values(Puzzle, a1),
  "29" = values(Puzzle, a4),
  Unit = [a1,a2,a3,a4,a5,a6,a7,a8,a9],
  [a1,a4] = places_for_value(Puzzle, Unit, $9),
  [a1,a4] = places_for_value(Puzzle, Unit, $2),
  ok.

test_is_solved() ->
  true = is_solved(solved_puzzle()),
  false = is_solved(empty_puzzle()),
  ok.

test_to_string() ->
  GridString = ".1736982563215894795872431682543
7169791586432346912758289643571573291684164875293",
    Puzzle = eliminate_digits(parse_grid(GridString), a1, "12356789"),
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
  lists:sort(dict:fetch_keys(ValuesDict)) =:= squares().

solved_puzzle() ->
  GridString = "41736982563215894795872431682543
7169791586432346912758289643571573291684164875293",
    parse_grid(GridString).

allTrue(Booleans) ->
  %% Test support function:
  %% Returns true if the list of booleans are all true.
  %% I expect there should already be such a function,
  %% please point me to it if you can.
  lists:all(fun(Bool) -> Bool end, Booleans).
