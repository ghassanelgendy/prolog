%%%%%%%%%%%%%%%%%%%%%%%%%%%% Teams Info %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Team(Name, Country, Num_of_winning_times)

team(barcelona, spain, 5).
team(real_madrid, spain, 6).
team(manchester_united, england, 3).
team(liverpool, england, 4).
team(juventus, italy, 7).
team(bayern_munich, germany, 8).
team(psg, france, 3).
team(inter_milan, italy, 3).
team(ajax, netherlands, 4).
team(porto, portugal, 7).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Players Info %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Player(Name, Team, Position)

player(messi, barcelona, forward).
player(ronaldo, real_madrid, forward).
player(pogba, manchester_united, midfielder).
player(salah, liverpool, forward).
player(modric, real_madrid, midfielder).
player(alisson, liverpool, goalkeeper).
player(ter_stegen, barcelona, goalkeeper).
player(varane, manchester_united, defender).
player(dybala, juventus, forward).
player(lewandowski, bayern_munich, forward).
player(neymar, psg, forward).
player(van_dijk, liverpool, defender).
player(ben_yedder, monaco, forward).
player(ronaldo_silva, manchester_city, midfielder).
player(de_jong, barcelona, midfielder).
player(ruben_dias, manchester_city, defender).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Matches Info %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Match(Team1, Team2, Team1Goals, Team2Goals)

match(barcelona, real_madrid, 3, 2).
match(manchester_united, liverpool, 1, 1).
match(real_madrid, manchester_united, 2, 0).
match(liverpool, barcelona, 2, 2).
match(juventus, inter_milan, 1, 2).
match(bayern_munich, psg, 3, 1).
match(ajax, porto, 2, 1).
match(manchester_city, juventus, 2, 2).
match(liverpool, bayern_munich, 0, 3).
match(real_madrid, ajax, 4, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Goals Info %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Goals(Player, Num_of_scored_goals)

goals(messi, 10).
goals(ronaldo, 12).
goals(pogba, 5).
goals(salah, 11).
goals(modric, 4).
goals(alisson, 0).
goals(dybala, 8).
goals(lewandowski, 9).
goals(neymar, 7).
goals(van_dijk, 2).
goals(ben_yedder, 6).
goals(ronaldo_silva, 3).
goals(de_jong, 4).
goals(ruben_dias, 1).
goals(ter_stegen, 0).
