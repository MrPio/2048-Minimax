game_size(3).
cpu_tile_val(2).

:- module(move_check, [move_check/3]).

apply_to_all(_, [], []).
apply_to_all(Pred, [H | T], [Result | RestResult]) :-
    call(Pred, H, Result),
    apply_to_all(Pred, T, RestResult).

% Transpose the board
transpose([], []).
transpose([[] | _], []).
transpose(B, [Row | Rows]) :-
    transpose_row(B, Row, RestB),
    transpose(RestB, Rows).

transpose_row([], [], []).
transpose_row([[X | Row] | Rows], [X | Col], [Row | RestRows]) :-
    transpose_row(Rows, Col, RestRows).

% Merge the row after a move (left, right, up, or down)
merge_row([], []).
merge_row([X], [X]).
merge_row([X, X | Rest], [M | Merged]) :-
    X \= 0,
    M is X*2,
    merge_row(Rest, Merged),!.
merge_row([X, Y | Rest], [X | MergedRest]) :-
    X \= Y,
    merge_row([Y | Rest], MergedRest),!.
merge_row(B, B).

shift_left(Row,NewRow):-
    length(Row, NewRowSize),
    game_size(GameSize),
    MissingZeros is max(0, GameSize - NewRowSize),
    length(ZeroList, MissingZeros),
    maplist(=(0), ZeroList),
    append(Row, ZeroList, NewRow).

remove_zeros(L,NewL):-
    exclude(==(0), L, NewL).

% Implementing the moves
% Admissible USER moves =========================================
move_left(B, NextB) :-
    apply_to_all(remove_zeros, B, NoZerosB),
    apply_to_all(merge_row, NoZerosB, MergeB),
    apply_to_all(shift_left,MergeB,NextB),
    \+ maplist(==, B, NextB),!. % checks if the board has changed

move_right(B, NextB) :-
    maplist(reverse, B, ReversedB),
    move_left(ReversedB, ReversedNextB),
    maplist(reverse, ReversedNextB, NextB).

move_up(B, NextB) :-
    transpose(B, TransposedB),
    move_left(TransposedB, TransposedNextB),
    transpose(TransposedNextB, NextB).

move_down(B, NextB) :-
    transpose(B, TransposedB),
    move_right(TransposedB, TransposedNextB),
    transpose(TransposedNextB, NextB).

% Admissible CPU moves =========================================

% Replaces a 0 in the board with the CPU tile
replace_one_zero([[0|T1]|BT], [[X|T1]|BT]):-
    cpu_tile_val(X).
replace_one_zero([[H|T1]|BT1], [[H|T2]|BT2]):-
   replace_one_zero([T1|BT1],[T2|BT2]).
replace_one_zero([[]|BT1],[[]|BT2]):-
    replace_one_zero(BT1,BT2).

% Make a CPU or USER move
move_check(user, B, NextB) :-
    move_left(B, NextB);
    move_right(B, NextB);
    move_up(B, NextB);
    move_down(B, NextB).
move_check(cpu, B, NextB) :-
    replace_one_zero(B,NextB).
