%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% P1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% P2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%hn3araff el city grid ely hanshta8al 3leh ay mxn
p2_grid(5, 5).

%el example ely fel slides
p2_cell(0, 0, empty).
p2_cell(0, 1, empty).
p2_cell(0, 2, package).
p2_cell(0, 3, empty).
p2_cell(0, 4, obstacle).
p2_cell(1, 0, empty).
p2_cell(1, 1, obstacle).
p2_cell(1, 2, empty).
p2_cell(1, 3, recharge).
p2_cell(1, 4, package).
p2_cell(2, 0, empty).
p2_cell(2, 1, empty).
p2_cell(2, 2, obstacle).
p2_cell(2, 3, package).
p2_cell(2, 4, empty).
p2_cell(3, 0, package).
p2_cell(3, 1, obstacle).
p2_cell(3, 2, empty).
p2_cell(3, 3, recharge).
p2_cell(3, 4, empty).
p2_cell(4, 0, empty).
p2_cell(4, 1, empty).
p2_cell(4, 2, empty).
p2_cell(4, 3, obstacle).
p2_cell(4, 4, empty).

%hn3araf el moves
p2_is_valid_move(Row, Col) :- %maynf3sh ytl3 bara el grid ,wala yroh 3la obstaacle
    p2_grid(MaxRow, MaxCol),
    Row >= 0,
    Row < MaxRow,
    Col >= 0,
    Col < MaxCol,
    p2_cell(Row, Col, Content),
    Content \= obstacle.

% hn3ml move 3shan n7rak eldrone , kol el directions f nafs ell function
% 3shan keda keda prolog haygrbhom kolohomm backtracking

p2_move((Row, Col), (NewRow, Col)) :- % Up
    NewRow is Row - 1,
    p2_is_valid_move(NewRow, Col). %etharak only if el move valid

p2_move((Row, Col), (NewRow, Col)) :- % Down
    NewRow is Row + 1,
    p2_is_valid_move(NewRow, Col).

p2_move((Row, Col), (Row, NewCol)) :- % Left
    NewCol is Col - 1,
    p2_is_valid_move(Row, NewCol).

p2_move((Row, Col), (Row, NewCol)) :- % Right
    NewCol is Col + 1,
    p2_is_valid_move(Row, NewCol).

%hnhadedd amaken kol el packages fel grid
p2_find_all_packages(Packages) :-
    findall((Row, Col), p2_cell(Row, Col, package), Packages).

%hnst5dm haga thseb el distance men el drone lel package
p2_distance((Row1, Col1), (Row2, Col2), Distance) :-
    RowDiff is abs(Row1 - Row2),
    ColDiff is abs(Col1 - Col2),
    Distance is RowDiff + ColDiff.

% hnbtdy n3rf ehna fen w el packages ely 3ayzynha fen
:- dynamic p2_initial_energy/1. %dynamic variable momken yt8ayar during runtime
p2_initial_state(StartState) :-
    p2_find_all_packages(Packages),
    write('Enter starting energy: '),
    read(Energy), %5odha user input
    retractall(p2_initial_energy(_)), %bysafarha 3shan law fy saved value men abl keda may3mlsh conflict
    assertz(p2_initial_energy(Energy)),
    StartState = state((0,0), [(0,0)], 0, Packages, Energy).

%tab 3ayzeen nwsl ly eh asln?
p2_goal(state(_, _, _, [],_)).

%3ayzeen haga tharakna men state lel tanya
p2_expand(state(Position, Path, Cost, PackagesLeft,Energy), NewState) :-
    Energy > 0,
    p2_move(Position, NewPosition),
    \+ member(NewPosition, Path),
    p2_update_state(state(Position, Path, Cost, PackagesLeft, Energy), NewPosition, NewState).

%3ayzeen haga t-update el state bta3tna b3d elharaka dy
p2_update_state(state(_, Path, Cost, PackagesLeft, Energy), NewPos, state(NewPos, [NewPos|Path], NewCost, NewPackagesLeft, NewEnergy)) :-
    ( member(NewPos, PackagesLeft) ->
        delete(PackagesLeft, NewPos, NewPackagesLeft) %law 3adena 3la package , shelha
    ;
        NewPackagesLeft = PackagesLeft
    ),
       ( NewPos = (Row, Col), p2_cell(Row, Col, recharge) ->
        p2_initial_energy(MaxEnergy),
        NewEnergy = MaxEnergy
    ;
        NewEnergy is Energy - 1
    ),
    NewCost is Cost + 1. % keda keda hnzwd el cost 3shan mshyna step

%3ayzeen nhseb priority ely hn5tar 3la asaha ahsan road
p2_priority(state(Position, _, CostSoFar, PackagesLeft,_), Priority) :-
    p2_closest_package_distance(Position, PackagesLeft, Heuristic),
    Priority is CostSoFar + Heuristic.

p2_closest_package_distance(_, [], 0). %base casa law el packages 5lst

p2_closest_package_distance(Position, [Package], Distance) :-  %law fadel package wahda bas ehseb elmasaafa lyha
    p2_distance(Position, Package, Distance).

p2_closest_package_distance(Position, [First|Rest], Distance) :- %llefely 3l packages men awl ellist lel a5r w  ehseb distance
    p2_distance(Position, First, D1), %ehseb distance ly awl package w smyha d1
    p2_closest_package_distance(Position, Rest, D2),  %shoof distnaces ly ba2y el  packages
    ( D1 < D2 -> Distance = D1 ; Distance = D2 ). %bn3tmed el distance elas8ar el min distance

p2_sort_by_priority(States, Sorted) :-
    map_list_to_pairs(p2_priority, States, Pairs), %bnhot priority ly kol state momkena
    keysort(Pairs, SortedPairs),%ascending sort according to ppriority
    pairs_values(SortedPairs, Sorted). %bnshyl el arqam w bnsyb el states bs

%BASE CASE El recursive function enna wsslna ly ely ehna 3ayzyno
p2_astar([State|_], State) :-
    p2_goal(State).

p2_astar([CurrentState | RestStates], Solution) :-
    findall(NewState, p2_expand(CurrentState, NewState), NewStates), %garab kol el possible moves wel states
    append(RestStates, NewStates, TempStates),%hotohm fel list
    p2_sort_by_priority(TempStates, SortedStates), %ratbhom
    p2_astar(SortedStates, Solution).%recursive baa lhd ma nwsl lel bassse case

p2_solve :-
    p2_initial_state(Start),
    p2_astar([Start], Solution),
    write('Final State: '), nl,
    write(Solution), nl.
