% 2048 with Minimax=======================================================
% The USER plays the MIN role, as such they want to minimize the number of empty
% tiles. On the countrary, the CPU plays the role of MAX, which tries to
% win the player by filling all the board tiles with numbers

%   -  -  -
%   4  -  2
%  16  2  -
%
:- use_module(move_check).
:- module(game, [move/2,
                 next_player/2,
                 turn_MIN/1,
                 turn_MAX/1,
                 benefit/2,
                 win/2]).

next_player(cpu,user).
next_player(user,cpu).

% move(+Pos,-NextPos) returns an admissible move from Pos to NextPos
move([P1, play, B], [_, win, NextB]) :- % CPU or USER won
    move_check(P1, B, NextB),
    win(P1, NextB), !.
move([P1, play, B], [P2, play, NextB]) :- % normal move
    next_player(P1, P2),
    move_check(P1, B, NextB).

% win(+Player, +Board)
% True if Player win in Board.
does_not_contain_zero(List) :-
    \+ member(0, List).
win(cpu, B):-
    maplist(does_not_contain_zero,B),
    \+ move_check(user,B,_),!.
win(user, B):-
    member(Row,B),
    member(2048,Row),!.

turn_MIN([cpu, _, _]).
turn_MAX([user, _, _]).

% Count the number of free tiles in the board
count_zeros(B, Count) :-
    flatten(B, FlatB),
    include(==(0), FlatB, Zeros),
    length(Zeros, Count).

% benefit(+Pos, -Val) Val is the benefit of the board B
benefit([_, _, B],  X):-
    points_2(B,X).
