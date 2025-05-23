

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
cell(1, 3, recharge).
cell(1, 4, package).
cell(2, 0, empty).
cell(2, 1, empty).
cell(2, 2, obstacle).
cell(2, 3, package).
cell(2, 4, empty).
cell(3, 0, package).
cell(3, 1, obstacle).
cell(3, 2, empty).
cell(3, 3, recharge).
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


%hnhadedd amaken kol el packages fel grid
find_all_packages(Packages) :-
    findall((Row, Col), cell(Row, Col, package), Packages).

%hnst5dm haga thseb el distance men el drone lel package
distance((Row1, Col1), (Row2, Col2), Distance) :-
    RowDiff is abs(Row1 - Row2),
    ColDiff is abs(Col1 - Col2),
    Distance is RowDiff + ColDiff.

% hnbtdy n3rf ehna fen w el packages ely 3ayzynha fen
:- dynamic initial_energy/1. %dynamic variable momken yt8ayar during runtime
initial_state(StartState) :-
    find_all_packages(Packages),
    write('Enter starting energy: '),
    read(Energy), %5odha user input
    retractall(initial_energy(_)), %bysafarha 3shan law fy saved value men abl keda may3mlsh conflict
    assertz(initial_energy(Energy)),
    StartState = state((0,0), [(0,0)], 0, Packages, Energy).

%tab 3ayzeen nwsl ly eh asln?
goal(state(_, _, _, [],_)).


%3ayzeen haga tharakna men state lel tanya
expand(state(Position, Path, Cost, PackagesLeft,Energy), NewState) :-
    Energy > 0,
    move(Position, NewPosition),
    \+ member(NewPosition, Path),
    update_state(state(Position, Path, Cost, PackagesLeft, Energy), NewPosition, NewState).

%3ayzeen haga t-update el state bta3tna b3d elharaka dy
update_state(state(_, Path, Cost, PackagesLeft, Energy), NewPos, state(NewPos, [NewPos|Path], NewCost, NewPackagesLeft, NewEnergy)) :-
    ( member(NewPos, PackagesLeft) ->
        delete(PackagesLeft, NewPos, NewPackagesLeft) %law 3adena 3la package , shelha
    ;
        NewPackagesLeft = PackagesLeft
    ),
       ( NewPos = (Row, Col), cell(Row, Col, recharge) ->
        initial_energy(MaxEnergy),
        NewEnergy = MaxEnergy
    ;
        NewEnergy is Energy - 1
    ),
    NewCost is Cost + 1. % keda keda hnzwd el cost 3shan mshyna step

%3ayzeen nhseb priority ely hn5tar 3la asaha ahsan road
priority(state(Position, _, CostSoFar, PackagesLeft,_), Priority) :-
    closest_package_distance(Position, PackagesLeft, Heuristic),
    Priority is CostSoFar + Heuristic.

closest_package_distance(_, [], 0). %base casa law el packages 5lst

closest_package_distance(Position, [Package], Distance) :-  %law fadel package wahda bas ehseb elmasaafa lyha
    distance(Position, Package, Distance).

closest_package_distance(Position, [First|Rest], Distance) :- %llefely 3l packages men awl ellist lel a5r w  ehseb distance
    distance(Position, First, D1), %ehseb distance ly awl package w smyha d1
    closest_package_distance(Position, Rest, D2),  %shoof distnaces ly ba2y el  packages
    ( D1 < D2 -> Distance = D1 ; Distance = D2 ). %bn3tmed el distance elas8ar el min distance


sort_by_priority(States, Sorted) :-
    map_list_to_pairs(priority, States, Pairs), %bnhot priority ly kol state momkena
    keysort(Pairs, SortedPairs),%ascending sort according to ppriority
    pairs_values(SortedPairs, Sorted). %bnshyl el arqam w bnsyb el states bs

%BASE CASE El recursive function enna wsslna ly ely ehna 3ayzyno
astar([State|_], State) :-
    goal(State).

astar([CurrentState | RestStates], Solution) :-
    findall(NewState, expand(CurrentState, NewState), NewStates), %garab kol el possible moves wel states
    append(RestStates, NewStates, TempStates),%hotohm fel list
    sort_by_priority(TempStates, SortedStates), %ratbhom
    astar(SortedStates, Solution).%recursive baa lhd ma nwsl lel bassse case


solve :-
    initial_state(Start),
    astar([Start], Solution),
    write('Final State: '), nl,
    write(Solution), nl.
