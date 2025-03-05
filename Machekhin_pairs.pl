initial_state(state([1,2,3], [1,2,3], [], [], left)).

goal_state(state([], [], RH, RW, right)) :-
    sort(RH, RHS), sort(RW, RWS), RHS=[1,2,3], RWS=[1,2,3].

safe_bank(H, W) :-
    W = [] ; 
     H = [] ; 
     forall(member(Wife, W), member(Wife, H)).

safe_state(state(LH, LW, RH, RW, _)) :-
    safe_bank(LH, LW),
    safe_bank(RH, RW).


move(state(LH, LW, RH, RW, left), state(LH1, LW1, RH1, RW1, right)) :-
    select_passengers(LH, LW, Passengers),
    update_banks(Passengers, LH, LW, RH, RW, LH1, LW1, RH1, RW1).

move(state(LH, LW, RH, RW, right), state(LH1, LW1, RH1, RW1, left)) :-
    select_passengers(RH, RW, Passengers),
    update_banks(Passengers, RH, RW, LH, LW, RH1, RW1, LH1, LW1).


select_passengers(H, _, [h(Person)]) :-
    member(Person, H).
select_passengers(_, W, [w(Person)]) :-
    member(Person, W).
select_passengers(H, _, [h(P1), h(P2)]) :-
    member(P1, H), member(P2, H), P1<P2.
select_passengers(_, W, [w(P1), w(P2)]) :-
    member(P1, W), member(P2, W), P1<P2.
select_passengers(H, W, [h(P1), w(P2)]) :-
    member(P1, H), member(P2, W), P1=P2.


update_banks(Passengers, H1, W1, H2, W2, H1New, W1New, H2New, W2New) :-
    remove_people(Passengers, H1, W1, H1Temp, W1Temp),
    add_people(Passengers, H2, W2, H2New, W2New),
    H1New = H1Temp,
    W1New = W1Temp.


remove_people([], H, W, H, W).
remove_people([h(P)|Ps], H, W, H1, W1) :-
    select(P, H, HTemp),
    remove_people(Ps, HTemp, W, H1, W1).
remove_people([w(P)|Ps], H, W, H1, W1) :-
    select(P, W, WTemp),
    remove_people(Ps, H, WTemp, H1, W1).


add_people([], H, W, H, W).
add_people([h(P)|Ps], H, W, H1, W1) :-
    add_people(Ps, [P|H], W, H1, W1).
add_people([w(P)|Ps], H, W, H1, W1) :-
    add_people(Ps, H, [P|W], H1, W1).


solve(Path) :-
    initial_state(Start),
    bfs([[Start]], [], Res), reverse(Res, Path).


bfs([[State|Path]|_], _, [State|Path]) :-
    goal_state(State).


bfs([Path|Paths], Visited, FinalPath) :-
    Path = [CurrentState|_],
    findall(
        [NextState|Path],
        (
            move(CurrentState, NextState),
            safe_state(NextState),
            \+ member(NextState, Visited),
            \+ (member([NextState|_], Paths))  
        ),
        NewPaths
    ),
    append(Paths, NewPaths, UpdatedPaths),
    append(Visited, [CurrentState], UpdatedVisited),
    bfs(UpdatedPaths, UpdatedVisited, FinalPath).



/** <examples>
?- solve(Path).
**/
