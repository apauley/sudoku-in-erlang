-module(sudoku).
-import(lists, [member/2, filter/2, map/2, flatmap/2, sort/1, all/2, sum/1]).
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

values(Puzzle, Square) ->
    {Dict, _} = Puzzle,
    dict:fetch(Square, Dict).

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

parse_grid(GridString) ->
    CleanGrid = clean_grid(GridString),
    81 = length(CleanGrid),
    parse_puzzle(empty_puzzle(), squares(), CleanGrid).

clean_grid(GridString) ->
    %% Return a string with only digits, 0 and .
    ValidChars = digits() ++ "0.",
    filter(fun(E) -> member(E, ValidChars) end, GridString).

parse_puzzle(Puzzle, [], []) ->
    Puzzle;
parse_puzzle(Puzzle, [Square|Squares], [Value|GridString]) ->
    {_,_} = Puzzle,
    IsDigit = member(Value, digits()),
    NewPuzzle = assign_if_digit(Puzzle, Square, Value, IsDigit),
    {_,_} = NewPuzzle,
    parse_puzzle(NewPuzzle, Squares, GridString).

assign_if_digit(Puzzle, Square, Value, true) ->
    %% Value is a Digit, possible to assign
    assign(Puzzle, Square, Value);
assign_if_digit(Puzzle, _, _, false) ->
    %% Not possible to assign
    Puzzle.

empty_puzzle() ->
    {empty_dict(), 0}.
empty_dict() ->
    Digits = digits(),
    dict:from_list([{Square, Digits} || Square <- squares()]).

assign(Puzzle, Square, Digit) ->
    %% Assign by eliminating all values except the assigned value.
    OtherValues = exclude_from(values(Puzzle, Square), Digit),
    eliminate_digits(Puzzle, Square, OtherValues).

eliminate_digits({false, Count}, _, _) ->
    {false, Count};
eliminate_digits(Puzzle, _, []) ->
    Puzzle;
eliminate_digits(Puzzle, Square, [Digit|T]) ->
    PuzzleOrFalse = eliminate(Puzzle, [Square], Digit),
    eliminate_digits(PuzzleOrFalse, Square, T).

eliminate({false, Count}, _, _) ->
    {false, Count};
eliminate(Puzzle, [], _) ->
    Puzzle;
eliminate({Dict, Count}, [Square|T], Digit) ->
    %% Eliminate the specified Digit from all specified Squares.
    Puzzle = {Dict, Count+1},
    OldValues = values(Puzzle, Square),
    NewValues = exclude_from(OldValues, Digit),
    NewPuzzle = eliminate(Puzzle, Square, Digit, NewValues, OldValues),
    eliminate(NewPuzzle, T, Digit).

eliminate({_, Count}, _, _, [], _) ->
    %% Contradiction: removed last value
    {false, Count};
eliminate(Puzzle, _, _, Vs, Vs) ->
    %% NewValues and OldValues are the same, already eliminated.
    Puzzle;
eliminate({ValuesDict, Eliminations}, Square, Digit, NewValues, _) ->
    NewDict = dict:store(Square, NewValues, ValuesDict),
    NewPuzzle = peer_eliminate({NewDict, Eliminations}, Square, NewValues),

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

assign_unique_place({false, Count}, _, _) ->
    {false, Count};
assign_unique_place(Puzzle, [], _) ->
    Puzzle;
assign_unique_place(Puzzle, [Unit|T], Digit) ->
    %% If a certain digit can only be in one place in a unit,
    %% assign it.
    Places = places_for_value(Puzzle, Unit, Digit),
    NewPuzzle = assign_unique_place_for_digit(Puzzle, Places, Digit),
    assign_unique_place(NewPuzzle, T, Digit).

assign_unique_place_for_digit({_, Count}, [], _) ->
    %% Contradiction: no place for Digit found
    {false, Count};
assign_unique_place_for_digit(Puzzle, [Square], Digit) ->
    %% Unique place for Digit found, assign
    assign(Puzzle, Square, Digit);
assign_unique_place_for_digit(Puzzle, _, _) ->
    %% Mutlitple palces (or none) found for Digit
    Puzzle.

places_for_value(Puzzle, Unit, Digit) ->
    [Square||Square <- Unit, member(Digit, values(Puzzle, Square))].

print_results(Filename, Seperator) ->
    {Time, Solutions} = timer:tc(sudoku, solve_file, [Filename, Seperator]),
    Solved = filter(fun(Puzzle) -> is_solved(Puzzle) end, Solutions),
    TimeInSeconds = Time/1000000,
    NumberPuzzles = length(Solutions),
    Hz = NumberPuzzles/TimeInSeconds,
    Eliminations = sum([Count|| {_, Count} <- Solutions]),
    EliminationsPerPuzzle = Eliminations/NumberPuzzles,
    Msg = "Solved ~p of ~p puzzles from ~s in ~f secs (~f Hz), ~p eliminations (~~~.2f per puzzle)~n",
    io:format(Msg,
              [length(Solved), NumberPuzzles, Filename, TimeInSeconds, Hz,
               Eliminations, EliminationsPerPuzzle]).

solve_file(Filename, Seperator) ->
    Solutions = solve_all(from_file(Filename, Seperator)),
    OutFilename = [filename:basename(Filename, ".txt")|".out"],
    ok = to_file(OutFilename, Solutions),
    Solutions.

solve_all(GridList) ->
    PidGrids = [{spawn(fun server/0), Grid}|| Grid <- GridList],
    map(fun({Pid, Grid}) -> Pid ! {self(), solve, Grid} end, PidGrids),
    map(fun receiveSolution/1, PidGrids).

receiveSolution({Pid, Grid}) ->
    receive
        {Pid, Grid, Solution} -> Solution
    end.

server() ->
    receive
        {From, solve, GridString} ->
            From ! {self(), GridString, solve(GridString)}
    end.

from_file(Filename, Seperator) ->
    {ok, BinData} = file:read_file(Filename),
    string:tokens(binary_to_list(BinData), Seperator).

to_file(Filename, Solutions) ->
    GridStrings = map(fun(S) -> [to_string(S)|"\n"] end, Solutions),
    ok = file:write_file(Filename, list_to_binary(GridStrings)).

is_solved(Puzzle) ->
    all(fun(Unit) -> is_unit_solved(Puzzle, Unit) end, unitlist()).
is_unit_solved(Puzzle, Unit) ->
    UnitValues = flatmap(fun(S) -> values(Puzzle, S) end, Unit),
    (length(UnitValues) == 9) and (sets:from_list(UnitValues) == sets:from_list(digits())).

solve(GridString) ->
    search(parse_grid(GridString)).

search({false, Count}) ->
    {false, Count};
search(Puzzle) ->
    search(Puzzle, is_solved(Puzzle)).
search(Puzzle, true) ->
    %% Searching an already solved puzzle should just return it unharmed.
    Puzzle;
search(Puzzle, false) ->
    {Square, Values} = least_valued_unassigned_square(Puzzle),
    first_valid_result(Puzzle, Square, Values).

least_valued_unassigned_square({ValuesDict, _}) ->
    Lengths = map(fun({S, Values}) -> {length(Values), S, Values} end,
                  dict:to_list(ValuesDict)),
    Unassigned = filter(fun({Length, _, _}) -> Length > 1 end, Lengths),
    {_, Square, Values} = lists:min(Unassigned),
    {Square, Values}.

to_string(Puzzle) ->
    {ValuesDict, _} = Puzzle,
    Fun = fun({_, [V]}) -> [V];
             ({_, _}) -> "."
          end,
    flatmap(Fun, sort(dict:to_list(ValuesDict))).

shallow_flatten([]) -> [];
shallow_flatten([H|T]) ->
    H ++ shallow_flatten(T).

exclude_from(Values, Digit) ->
    lists:delete(Digit, Values).

%% Returns the first non-false puzzle, otherwise false
first_valid_result({_, Count}, _, []) ->
    {false, Count};
first_valid_result(Puzzle, Square, [Digit|T]) ->
    PuzzleOrFalse = search(assign(Puzzle, Square, Digit)),
    first_valid_result(Puzzle, Square, [Digit|T], PuzzleOrFalse).
first_valid_result({Dict, ValidCount}, Square, [_|T], {false, InvalidCount}) ->
    first_valid_result({Dict, ValidCount+(InvalidCount-ValidCount)}, Square, T);
first_valid_result(_, _, _, Puzzle) ->
    Puzzle.
