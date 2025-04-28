%hn3araff el city grid ely hanshta8al 3leh ay mxn

grid(5, 5).

%el example ely fel slides
cell(0, 0, empty).
cell(0, 1, empty).
cell(0, 2, package).
cell(0, 3, empty).
cell(0, 4, obstacle).
cell(1, 0, empty).
cell(1, 1, obstacle).
cell(1, 2, empty).
cell(1, 3, empty).
cell(1, 4, package).
cell(2, 0, empty).
cell(2, 1, empty).
cell(2, 2, obstacle).
cell(2, 3, package).
cell(2, 4, empty).
cell(3, 0, package).
cell(3, 1, obstacle).
cell(3, 2, empty).
cell(3, 3, empty).
cell(3, 4, empty).
cell(4, 0, empty).
cell(4, 1, empty).
cell(4, 2, empty).
cell(4, 3, obstacle).
cell(4, 4, empty).

%hn3araf el moves
is_valid_move(Row, Col) :- %maynf3sh ytl3 bara el grid ,wala yroh 3la obstaacle
    grid(MaxRow, MaxCol),
    Row >= 0,
    Row < MaxRow,
    Col >= 0,
    Col < MaxCol,
    cell(Row, Col, Content),
    Content \= obstacle.

% hn3ml move 3shan n7rak eldrone , kol el directions f nafs ell function
% 3shan keda keda prolog haygrbhom kolohomm backtracking

move((Row, Col), (NewRow, Col)) :- % Up
    NewRow is Row - 1,
    is_valid_move(NewRow, Col). %etharak only if el move valid

move((Row, Col), (NewRow, Col)) :- % Down
    NewRow is Row + 1,
    is_valid_move(NewRow, Col).

move((Row, Col), (Row, NewCol)) :- % Left
    NewCol is Col - 1,
    is_valid_move(Row, NewCol).

move((Row, Col), (Row, NewCol)) :- % Right
    NewCol is Col + 1,
    is_valid_move(Row, NewCol).

%3ayzeen variable yb2a fyh position w halet el drone
state(Position, Path, CostSoFar, PackagesLeft).

%hnhadedd amaken kol el packages fel grid
find_all_packages(Packages) :-
    findall((Row, Col), cell(Row, Col, package), Packages).

%hnst5dm haga thseb el distance men el drone lel package
distance((Row1, Col1), (Row2, Col2), Distance) :-
    RowDiff is abs(Row1 - Row2),
    ColDiff is abs(Col1 - Col2),
    Distance is RowDiff + ColDiff.

% hnbtdy n3rf ehna fen w el packages ely 3ayzynha fen
initial_state(StartState) :-
    find_all_packages(Packages),
    StartState = state((0,0), [(0,0)], 0, Packages).

%tab 3ayzeen nwsl ly eh asln?
goal(state(_, _, _, [])).


%3ayzeen haga tharakna men state lel tanya
expand(state(Position, Path, Cost, PackagesLeft), NewState) :-
    move(Position, NewPosition),
    update_state(state(Position, Path, Cost, PackagesLeft), NewPosition, NewState).

%3ayzeen haga t-update el state bta3tna b3d elharaka dy
update_state(state(_, Path, Cost, PackagesLeft), NewPos, state(NewPos, [NewPos|Path], NewCost, NewPackagesLeft)) :-
    ( member(NewPos, PackagesLeft) ->
        delete(PackagesLeft, NewPos, NewPackagesLeft) %law 3adena 3la package , shelha
    ;
        NewPackagesLeft = PackagesLeft
    ),
    NewCost is Cost + 1. % keda keda hnzwd el cost 3shan mshyna step

%3ayzeen nhseb priority ely hn5tar 3la asaha ahsan road
priority(state(Position, _, CostSoFar, PackagesLeft), Priority) :-
    closest_package_distance(Position, PackagesLeft, Heuristic),
    Priority is CostSoFar + Heuristic.


closest_package_distance(Position, [First|Rest], Distance) :- %llefely 3l packages men awl ellist lel a5r w  ehseb distance
    distance(Position, First, D1), %ehseb distance ly awl package w smyha d1
    closes_package_distance(Position, Rest, D2),  %shoof distnaces ly ba2y el  packages
    ( D1 < D2 -> Distance = D1 ; Distance = D2 ). %bn3tmed el distance elas8ar el min distance
closest_package_distance(_, [], 0). %base casa law el packages 5lst

sort_by_priority(States, Sorted) :-
    map_list_to_pairs(priority, States, Pairs), %bnhot priority ly kol state momkena
    keysort(Pairs, SortedPairs),%ascending sort according to ppriority
    pairs_values(SortedPairs, Sorted). %bnshyl el arqam w bnsyb el states bs

astar([CurrentState | RestStates], Solution) :-
    findall(NewState, expand(CurrentState, NewState), NewStates),%garab kol el possible moves wel states
    append(RestStates, NewStates, AllStates), %hotohom fel list
    sort_by_priority(AllStates, SortedStates),%ratebhomm
    astar(SortedStates, Solution).%recursive lhd ma nwsal el base case



%BASE CASE El recursive function enna wsslna ly ely ehna 3ayzyno
astar([State|_], State) :-
    goal(State).
