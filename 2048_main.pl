:- use_module(minimax).
:- use_module(game).
% :- use_module(alfabeta).

% PLAY ==============================================================
play :-
    nl,
    write('===================='), nl,
    write('= Prolog 2048 ==='), nl,
    write('===================='), nl, nl,
    Board=[
       [0,0,0],
        [0,0,0],
        [0,0,0]
    ],
    play([cpu,play,Board]).

% play(+Position) =====================================
play([user, play, Board]) :-
    bestMove([user,play,Board],[NextPlayer,State,NextBoard]),
    show(NextBoard),
    (
      State = win, !,
      nl, write('The AI won the game!'),nl,nl
      ;
     %      true
      play([NextPlayer, play, NextBoard])
    ).
play([cpu, play, Board]) :-
    % Select one random CPU move from all the available ones
    %get_time(T1),
    findall([NextPlayer,State,NextBoard],
            move([cpu,play,Board],[NextPlayer,State,NextBoard]),
            Moves),
    random_member([NextPlayer,State,NextBoard],Moves),
    %get_time(T2),
    %ElapsedTime is T2 - T1, % < 1ms
    %format('Elapsed time: ~3f seconds~n', [ElapsedTime]),

    nl, %show(NextBoard),
    (
      State = win, !,
      normal_points(Board,Points),
      nl, write('Game over'),nl,
            write('points='),write(Points),nl
      ;
      play([NextPlayer, play, NextBoard])
    ).

bestMove(Pos, NextPos) :-
    %alfabeta(Pos,-5,+5,NextPos, _).
    minimax(Pos, NextPos, _,0).

% show(+Board)
show(Board) :-
    maplist(writeln,Board).

% show2(+Termine)
show2(X) :-
    X = 0, !,
    write(' ').

show2(X) :-
    write(X).

% Points calculation ==========================================
square(X, Result) :- Result is X * X.

points_1(Board,Points):-
    flatten(Board,Numbers),
    maplist(square, Numbers, Squares),
    sum_list(Squares, Points).

scalar_product(List1, List2, Result) :-
    maplist(product, List1, List2, Products),
    sum_list(Products, Result).
product(X, Y, P) :- P is X * Y.

pow(X, P) :- P is X.
points_2(Board,Points):-
    WeightMatrix=[
        [3,2,1],
        [4,5,6],
        [9,8,7]
        %[16,8,4],
       % [32,64,128],
       % [1024,512,256]
    ],
    flatten(WeightMatrix,Weights),
    maplist(pow,Weights,PowWeights),
    flatten(Board,Numbers),
    scalar_product(Numbers,PowWeights,Points).

normal_points(Board,Points):-
    flatten(Board,Numbers),
    sum_list(Numbers, Points).


