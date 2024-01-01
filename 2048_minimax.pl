:- module(minimax, [minimax/4]).
% minimax(+Pos, -MigliorPosSucc, -Val, Depth)============= Pos è lo
% stato del gioco, vengono restituiti il suo Val minimax e la
% MigliorPosSucc, ovvero la mossa da fare
minimax(Pos, BestNextPos, Val, Depth) :-
    Depth =< 5,
    %tabula(Depth),write(Pos),write('  Depth='),write(Depth),nl,
    bagof(NextPos, move(Pos, NextPos), NextPosList),!,
    length(NextPosList,NextPosListSize),%tabula(Depth),writeln(NextPosListSize),
    [Player,_,_]=Pos,
    (
                     Player=cpu,
                     %trace,
                     findall(V,choose_one_pos(NextPosList,V,Depth),ValList),
                     sum_list(ValList,ValSum),
                     Val is ValSum/NextPosListSize
                     %write('Val='),write(Val)
                     ;
                     Player=user,
                     best(NextPosList,BestNextPos , Val, Depth)
    ).
minimax(Pos, _, Val, Depth) :-
    benefit(Pos, Val).
    %tabula(Depth),write(Pos),write('  Depth='),write(Depth), write('  Val='), write(Val),nl.

choose_one_pos(NextPosList,Val,Depth):-
        member(CpuPos,NextPosList),
        D is Depth+1,
        minimax(CpuPos,_,Val,D).


% best(+NextPosList, -BestNextPos, -Val) =========================
% returns BestNextPos and its value from the NextPosList
best([Pos], Pos, Val, Depth) :-
    NewDepth is Depth + 1,
    minimax(Pos, _, Val, NewDepth),!.
best([Pos1|NextPosList], BestNextPos, BestPosVal, Depth) :-
    NewDepth is Depth + 1,
    minimax(Pos1, _, Val1,NewDepth),
    best(NextPosList, Pos2, Val2, Depth),
    betterThan(Pos1, Val1, Pos2, Val2, BestNextPos, BestPosVal).

% betterThan(+Pos0, +Val0, +Pos1, +Val1, -Pos, -Val) ==============
% Returns the best between the two given (Pos,Val) couples
betterThan(Pos0,Val0, _,Val1, Pos0,Val0) :-
    turn_MIN(Pos0),
    Val0 > Val1, !
    ;
    turn_MAX(Pos0),
    Val0 < Val1, !.
betterThan(_,_,Pos1,Val1,Pos1,Val1).

tabula(Depth):-Depth=<0,!.
tabula(Depth):-write('\t'),D is Depth-1,tabula(D).

