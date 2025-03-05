transfer(l, r).
transfer(r, l).

move([A, Wolf, Goat, Cabbage], nothing, [B, Wolf, Goat, Cabbage]) :- transfer(A, B).
move([A, A, Goat, Cabbage], wolf, [B, B, Goat, Cabbage]) :- transfer(A, B).
move([A, Wolf, A, Cabbage], goat, [B, Wolf, B, Cabbage]) :- transfer(A, B).
move([A, Wolf, Goat, A], cabbage, [B, Wolf, Goat, B]) :- transfer(A, B).

safeState([Man, Wolf, Goat, Cabbage]) :- (Man = Wolf ; Wolf\=Goat), (Man=Goat ; Goat\=Cabbage).

solution([r,r,r,r], _, []).
solution(State, PrevStates, [Move | OtherMoves]) :-
    move(State, Move, NextState), safeState(NextState), (\+ member(NextState,PrevStates)), 
    %write(PrevStates), write("\n") ,
    append(PrevStates, [NextState], NPS),
    solution(NextState, NPS, OtherMoves).

allSolutions(InitialState, Solutions) :-
    findall(Path, solution(InitialState, [InitialState], Path), AllSolutions), sort(AllSolutions, Solutions).

/** <examples>
?- solution([l,l,l,l], [[l,l,l,l]], X).
?- allSolutions([l,l,l,l], X).
**/