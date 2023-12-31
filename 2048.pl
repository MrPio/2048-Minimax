% 2048 Minimax ==========================================================
% The USER plays the MIN role, as such they want to minimize the number
% of empty tiles. On the countrary, the CPU plays the role of MAX, which 
% tries to win the player by filling all the board tiles with numbers

%   -  -  -
%   4  -  2
%  16  2  -

% move(+Pos,-NextPos) returns an admissible move from Pos to NextPos
move([cpu, play, Board], [user, win, NextBoard]) :- % CPU wins
    move_check(cpu, Board, NextBoard),
    vincente(cpu, NextBoard), !.
move([P1, play, Board], [P2, deadlock, NextBoard]) :- % deadlock
    next_player(P1, P2),
    move_check(P1, Board, NextBoard),
    deadlock(P1, NextBoard), !.
move([P1, play, Board], [P2, play, NextBoard]) :- % normal move
    next_player(P1, P2),
    move_check(P1, Board, NextBoard).

% move_check(+Player, +Board, -NextBoard)
% NextBoard is Board with free tile filled by USER
move_check(cpu, [0|T], [2|T]).
move_check(cpu, [B|T], [B|T2]) :-
    move_check(cpu, T, T2).

% vincente(+Player, +Board)
% True if Player win in Board.







