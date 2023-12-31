% 2048 Minimax ==========================================================
% The USER plays the MIN role, as such they want to minimize the number
% of empty tiles. On the contrary, the CPU plays the role of MAX, which 
% tries to win the player by filling all the board tiles with numbers

%   -  -  -
%   4  -  2
%  16  2  -

% move(+Pos,-NextPos) returns an admissible move from Pos to NextPos
move([cpu, play, Board], [user, win, NewBoard]) :- % CPU wins
    move_check(cpu, Board, NewBoard),
    vincente(cpu, NewBoard), !.
move([P1, play, Board], [P2, deadlock, NewBoard]) :- % deadlock
    next_player(P1, P2),
    move_check(P1, Board, NewBoard),
    deadlock(P1, NewBoard), !.
move([P1, play, Board], [P2, play, NewBoard]) :- % normal move
    next_player(P1, P2),
    move_check(P1, Board, NewBoard).

% move_check(+Player, +Board, -NextBoard)
% NextBoard is Board with free tile filled by USER
move_check(P, [0|Bs], [P|Bs]).
move_check(P, [B|Bs], [B|B2s]) :-
    move_check(P, Bs, B2s).

% vincente(+Player, +Board)
% True if Player win in Board.







