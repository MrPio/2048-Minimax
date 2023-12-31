game_size(3).

% Helper predicates to split and merge rows
apply_to_all(_, [], []).
apply_to_all(Pred, [H | T], [Result | RestResult]) :-
    call(Pred, H, Result),
    apply_to_all(Pred, T, RestResult).

% Split a list into two parts at the specified index
split_at(Index, List, Prefix, Suffix) :-
    length(Prefix, Index),
    append(Prefix, Suffix, List).

% Transpose a matrix
transpose([], []).
transpose([[] | _], []).
transpose(Matrix, [Row | Rows]) :-
    transpose_row(Matrix, Row, RestMatrix),
    transpose(RestMatrix, Rows).

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
    exclude(==(0), Row, TrimRow),
    length(TrimRow, NewRowSize),
	game_size(GameSize),
    MissingZeros is max(0, GameSize - NewRowSize),
    length(ZeroList, MissingZeros),
    maplist(=(0), ZeroList),
    append(TrimRow, ZeroList, NewRow).

% Implementing the 2048 game moves ======================================
% Admissible move to the left
move_left(B, NextB) :-
    apply_to_all(merge_row,B,MergeB),
    apply_to_all(shift_left,MergeB,NextB),
    \+ maplist(==, B, NextB). % checks if the board has changed

% ERROR!
% move_left( [[0,16,16],[2,0,2],[4,8,0]], NextB).

% Admissible move to the right
move_right(Board, NewBoard) :-
    maplist(reverse, Board, Reversed),
    move_left(Reversed, TempBoard),
    maplist(reverse, TempBoard, NewBoard).

% Admissible move upwards
move_up(Board, NewBoard) :-
    transpose(Board, Transposed),
    move_left(Transposed, TempBoard),
    transpose(TempBoard, NewBoard).

% Admissible move downwards
move_down(Board, NewBoard) :-
    transpose(Board, Transposed),
    move_right(Transposed, TempBoard),
    transpose(TempBoard, NewBoard).

% Predicate to check if the second board is obtainable from the first
is_obtainable(B, NextB) :-
    move_left(B, NextB),
    move_right(B, NextB),
    move_up(B, NextB),
    move_down(B, NextB).
