-module(sudoku).

-import(lists,
        [member/2, filter/2, map/2, flatmap/2, sort/1, all/2,
         sum/1]).

-compile(export_all).

-compile({parse_transform, ct_expand}).

-define(digits, "123456789").

-define(rows, "abcdefghi").

-define(cols, ?digits).

squares() ->
    %% Returns a list of 81 square names, including "a1" etc.
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
    NonUniquePeers = shallow_flatten([S
                                      || S <- units(Square)]),
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
    Lengths = [least_valued_unassigned_square_2(V)
               || V <- dict:to_list(ValuesDict)],
    Unassigned = [V1
                  || V1 <- Lengths, least_valued_unassigned_square_1(V1)],
    {_, Square, Values} = lists:min(Unassigned),
    {Square, Values}.

least_valued_unassigned_square_1({Length, _, _}) ->
    Length > 1.

least_valued_unassigned_square_2({S, Values}) ->
    {length(Values), S, Values}.

solve_all(GridList) ->
    PidGrids = [{spawn(fun server/0), Grid}
                || Grid <- GridList],
    [solve_all_1(V) || V <- PidGrids],
    [receiveSolution(V1) || V1 <- PidGrids].

solve_all_1({Pid, Grid}) -> Pid ! {self(), solve, Grid}.

receiveSolution({Pid, Grid}) ->
    receive {Pid, Grid, Solution} -> Solution end.

server() ->
    receive
      {From, solve, GridString} ->
          From ! {self(), GridString, solve(GridString)}
    end.

is_solved(Puzzle) ->
    lists:all(fun (Unit) -> is_unit_solved(Puzzle, Unit)
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
    GridStrings = [[to_string(S) | "\n"] || S <- Solutions],
    ok = file:write_file(Filename,
                         list_to_binary(GridStrings)).

solve_file(Filename, Seperator) ->
    Solutions = solve_all(from_file(Filename, Seperator)),
    OutFilename = [filename:basename(Filename, ".txt")
                   | ".out"],
    ok = to_file(OutFilename, Solutions),
    Solutions.

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

shallow_flatten([]) -> [];
shallow_flatten([H | T]) -> H ++ shallow_flatten(T).

exclude_from(Values, Digit) ->
    lists:delete(Digit, Values).
