:- dynamic(grid/1).

% Possible movement directions: right, left, down, up
move([X, Y], [NX, Y]) :- NX is X + 1.
move([X, Y], [NX, Y]) :- NX is X - 1.
move([X, Y], [X, NY]) :- NY is Y + 1.
move([X, Y], [X, NY]) :- NY is Y - 1.

% Ensure a position is within grid boundaries
valid_position(X, Y) :-
    grid(Grid),
    length(Grid, NumRows),
    nth0(0, Grid, FirstRow),
    length(FirstRow, NumCols),
    X >= 0, Y >= 0, X < NumRows, Y < NumCols.

% Fetch value at a specific cell
get_cell(X, Y, Value) :-
    grid(Grid),
    nth0(X, Grid, Row),
    nth0(Y, Row, Value).

% Replace an element at a specific index in a list
update_row([_|Tail], 0, NewVal, [NewVal|Tail]).
update_row([Head|Tail], Index, NewVal, [Head|UpdatedTail]) :-
    Index > 0,
    NewIndex is Index - 1,
    update_row(Tail, NewIndex, NewVal, UpdatedTail).

% Replace a cell in the grid with a new value
update_grid(Grid, X, Y, NewVal, NewGrid) :-
    nth0(X, Grid, Row),
    update_row(Row, Y, NewVal, NewRow),
    update_row(Grid, X, NewRow, NewGrid).

% Display the grid on the console
display_grid(Grid) :-
    nl,
    forall(member(Row, Grid),
           (forall(member(Cell, Row), (write(Cell), write(' '))), nl)), nl.

% Show each step in the path with updated markers
show_path(_, []).
show_path(Grid, [[X,Y]|RestPath]) :-
    update_grid(Grid, X, Y, '*', UpdatedGrid),
    display_grid(UpdatedGrid),
    show_path(UpdatedGrid, RestPath).

% Helper predicates
valid_position_pair([X, Y]) :- valid_position(X, Y).
get_cell_value([X, Y], Value) :- get_cell(X, Y, Value).

% Breadth-First Search implementation to find the best delivery path
bfs(Queue, BestPath, MaxDelivered) :-
    bfs_loop(Queue, [], 0, BestPath, MaxDelivered).

bfs_loop([], BestSoFar, MaxSoFar, BestSoFar, MaxSoFar).

bfs_loop([[CurrentPos, PathSoFar, Visited, PackagesDelivered]|RestQueue], BestPath, MaxSoFar, FinalPath, FinalDelivered) :-
    findall(
        [NextPos, [NextPos|PathSoFar], [NextPos|Visited], UpdatedDelivered],
        (
            move(CurrentPos, NextPos),
            valid_position_pair(NextPos),
            \+ member(NextPos, Visited),
            \+ get_cell_value(NextPos, 'O'),
            (get_cell_value(NextPos, 'P') -> UpdatedDelivered is PackagesDelivered + 1 ; UpdatedDelivered = PackagesDelivered)
        ),
        NextMoves
    ),
    append(RestQueue, NextMoves, NewQueue),
    (
        PackagesDelivered > MaxSoFar
        -> bfs_loop(NewQueue, PathSoFar, PackagesDelivered, FinalPath, FinalDelivered)
        ;  bfs_loop(NewQueue, BestPath, MaxSoFar, FinalPath, FinalDelivered)
    ).

% Mark the full path with '*' and the final drone position with 'D'
mark_final_path(Grid, Path) :-
    mark_travel_path(Grid, Path, TempGrid),
    last(Path, [FinalX, FinalY]),
    update_grid(TempGrid, FinalX, FinalY, 'D', MarkedGrid),
    display_grid(MarkedGrid).

mark_travel_path(Grid, [], Grid).
mark_travel_path(Grid, [[X,Y]|Rest], UpdatedGrid) :-
    update_grid(Grid, X, Y, '*', TempGrid),
    mark_travel_path(TempGrid, Rest, UpdatedGrid).

% Prompt user to enter a positive integer
prompt_positive_integer(Message, Number) :-
    write(Message),
    read_line_to_string(user_input, Input),
    number_string(Number, Input),
    integer(Number),
    Number > 0, !.
prompt_positive_integer(Message, Number) :-
    write('Invalid input! Please enter a positive integer: '), nl,
    prompt_positive_integer(Message, Number).

% Read a single cell from the user
read_cell_input(Row, Col, Cell) :-
    format('Row ~w, Column ~w [D/P/O/-]: ', [Row, Col]),
    read_line_to_string(user_input, Input),
    ( Input = "" -> Cell = '-' ; string_chars(Input, [Char|_]), atom_string(Cell, Char) ),
    member(Cell, ['D', 'P', 'O', '-']), !.
read_cell_input(Row, Col, Cell) :-
    write('Invalid input! Please enter D, P, O, or -: '), nl,
    read_cell_input(Row, Col, Cell).

% Read an entire row of cells
read_row_input(ColCount, RowIndex, Row) :-
    collect_row_cells(1, ColCount, RowIndex, [], Collected),
    reverse(Collected, Row).

% Collect cells in a row
collect_row_cells(CurrentCol, TotalCols, _, Acc, Acc) :-
    CurrentCol > TotalCols, !.
collect_row_cells(CurrentCol, TotalCols, RowNum, Acc, Row) :-
    read_cell_input(RowNum, CurrentCol, Cell),
    NextCol is CurrentCol + 1,
    collect_row_cells(NextCol, TotalCols, RowNum, [Cell|Acc], Row).

% Read full grid from user
read_entire_grid(RowCount, ColCount, Grid) :-
    collect_rows(1, RowCount, ColCount, [], CollectedRows),
    reverse(CollectedRows, Grid).

% Accumulate rows
collect_rows(CurrentRow, TotalRows, _, Acc, Acc) :-
    CurrentRow > TotalRows, !.
collect_rows(CurrentRow, TotalRows, Cols, Acc, Grid) :-
    format('Entering row ~w:~n', [CurrentRow]),
    read_row_input(Cols, CurrentRow, Row),
    NextRow is CurrentRow + 1,
    collect_rows(NextRow, TotalRows, Cols, [Row|Acc], Grid).

% Ask user to define grid size and contents
setup_grid :-
    prompt_positive_integer('Enter number of rows: ', Rows),
    prompt_positive_integer('Enter number of columns: ', Cols),
    retractall(grid(_)),
    write('Enter the grid (top to bottom, left to right):'), nl,
    read_entire_grid(Rows, Cols, Grid),
    assertz(grid(Grid)).

% Solve and run the drone delivery simulation
solve :-
    setup_grid,
    grid(InitialGrid),
    bfs([[[0,0], [[0,0]], [[0,0]], 0]], Path, DeliveredCount),
    reverse(Path, OrderedPath),
    write('Initial Grid:'), nl,
    display_grid(InitialGrid),
    write('Movement Steps:'), nl,
    show_path(InitialGrid, OrderedPath),
    write('Final Grid State:'), nl,
    mark_final_path(InitialGrid, OrderedPath),
    write('Total Packages Delivered: '), write(DeliveredCount), nl.
