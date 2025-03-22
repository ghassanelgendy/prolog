:- consult(league_data).

%%%%%%%%%%%%%%%%%%%%%%%%%%% TASK 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Get a list of all players in a specific team %%%%%%%%%%%%%%%%%%%%%%%%%%%

players_in_team(Team, Players) :-
    gather_players(Team, [], Acc),    % Beylemhom fe list
    reverse(Acc, Players),!.          % Reverse 3shan beylmhom backtracking

% Recursive Predicate To Gather Players In a team.
gather_players(Team, Acc, Players) :-
    player(P, Team, _),              % Find a player in the given team
    \+ member(P, Acc),!,             % En elesm da msh mawgoud fellist
    gather_players(Team, [P|Acc], Players). % Continue collecting players

% Base case: When no more players are left, return the accumulated list.
gather_players(_, Acc, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%% TASK 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Count how many teams are from a specific country %%%%%%%%%%%%%%%%%%%%%%%%%%%

count_teams_in_country(Country, Count) :-
    find_teams(Country, [], Teams),    % Collect unique teams in a list
    length(Teams, Count),!.            % Count the number of collected teams

% Base case: Stop when no more teams exist in the given country.
find_teams(Country, Acc, Acc) :-
    \+ (team(Team, Country, _), \+ member(Team, Acc)).  % Stop when all teams are collected.

% Recursive case: If a team belongs to the country and is not counted yet, add it.
find_teams(Country, Acc, Teams) :-
    team(Team, Country, _),        % Find a team in the given country
    \+ member(Team, Acc),          % Msh mawgouda fel acc list
    find_teams(Country, [Team | Acc], Teams).  % Continue collecting teams mangher elHead


%%%%%%%%%%%%%%%%%%%%%%%%%%% TASK 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Find the team with the most championship titles %%%%%%%%%%%%%%%%%%%%%%%%%%%

% Main predicate: Find the most successful team.
most_successful_team(Team) :-
    team(Team, _, Titles),              % Start with any team
    \+ (team(_, _, OtherTitles), OtherTitles > Titles),
    !.  % Ensure no team has more titles

%%%%%%%%%%%%%%%%%%%%%%%%%%% TASK 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% List all matches where a specific team participated %%%%%%%%%%%%%%%%%%%%%%%%%%%

% Main predicate: Collect all matches where a given team participated.
matches_of_team(Team, Matches) :-
    collect_matches(Team, Matches, []).

% Recursive clause for when Team appears as Team1.
collect_matches(Team, Matches, Acc) :-
    match(Team, Opponent, G1, G2),
    \+ member((Team, Opponent, G1, G2), Acc),
    collect_matches(Team, Matches, [(Team, Opponent, G1, G2) | Acc]),
    !. %3shan nstop elbacktracking ely malosh lazma (bonus)

% Recursive clause for when Team appears as Team2.
collect_matches(Team, Matches, Acc) :-
    match(Opponent, Team, G1, G2),
    \+ member((Opponent, Team, G1, G2), Acc),
    collect_matches(Team, Matches, [(Opponent, Team, G1, G2) | Acc]),
    !. %3shan nstop elbacktracking ely malosh lazma (bonus)
% Base case: no more matches can be added.
collect_matches(_, Matches, Matches).



%%%%%%%%%%%%%%%%%%%%%%%%%%% TASK 5 %%%%%%%%%%%%%%%%%%%%%%%%%%%

append([], L, L). %law el list  fadya hot L
append([H|T], L, [H|R]) :- append(T, L, R). %law fy list fyha H w T zawed 3lehom L

num_matches_of_team(Team, Count) :-
    gather_matches(Team, [], Matches), %function btlem el matchees w thotohom f list
    length(Matches, Count). %bnshoof hagm el  list dy

gather_matches(Team, Acc, Res) :-
    match(Team, Opp, GT, GO),%btdawar 3l matches ely el team l3b fyha
    \+ member(match(Team, Opp, GT, GO), Acc), %btt2aked en el match mesh mahtot abl keda
    gather_matches(Team, [match(Team, Opp, GT, GO)|Acc], Res). %bnhotaha el list b3den ben-calll tany el function recursively
gather_matches(Team, Acc, Res) :- %btdawar fel matches ely el team fyha ka second parameter
    match(Opp, Team, GO, GT),
    \+ member(match(Opp, Team, GO, GT), Acc),
    gather_matches(Team, [match(Opp, Team, GO, GT)|Acc], Res).
gather_matches(_, Acc, Acc).%lama btb2a el matches 5lst 5alaas
%%%%%%%%%%%%%%%%%%%%%%%%%%% TASK 6 %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Find the top goal scorer %%%%%%%%%%%%%%%%%%%%%%%%%%%

% Main predicate: Find the player with the most goals.
top_scorer(Player) :-
    goals(Player, Goals),                  % Get a player's goals
    \+ (goals(_, OtherGoals), OtherGoals > Goals),  % Ensure no player has more goals
    !.  % Stop unnecessary backtracking

%%%%%%%%%%%%%%%%%%%%%%%%%%% TASK 7 %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% Find the Most Common Position in a Specific Team %%%%%%%%%%%%%%%%%%%%%%%%%%%


most_common_position_in_team(Team, Position) :-
    player(_, Team, Position),              % Pick a candidate position from the team
    frequency(Team, Position, Count),       % Count how many players have that position
    \+ ( player(_, Team, OtherPos),         % For any other position in the team,
         frequency(Team, OtherPos, OtherCount),
         OtherCount > Count ),              % ensure no other position has a higher count
    !.                                      % Stop backtracking once the best is found

% frequency/3 counts how many players in a team play in a given position.
frequency(Team, Position, Count) :-
    frequency_helper(Team, Position, 0, Count).

% frequency_helper/4 uses a failure-driven loop:
% For each solution of player(_,Team,Position), increment the accumulator,
% then force backtracking with 'fail'. When no more players are found, succeed with the final Count.
frequency_helper(Team, Position, Acc, Count) :-
    player(_, Team, Position),
    NewAcc is Acc + 1,
    fail.
frequency_helper(_, _, Count, Count).
