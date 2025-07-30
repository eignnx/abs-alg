:- use_module(library(clpfd)).
:- encoding(utf8).

cycle(cycle:[]).
cycle(cycle:[X | Xs]) :- X in 1..sup, cycle(cycle:Xs).

% perm:[2, 4, 3, 1] maps 1->2, 2->4, 3->3, 4->1.
is_permutation(perm:Xs) :-
    phrase(indices(_), Is),
    phrase(permutation(Is), Xs).

portray(perm:Xs) :-
    is_list(Xs),
    length(Xs, N),
    format("~n/"),
    phrase(indices(N), Is),
    maplist([X]>>format(" ~p", [X]), Is),
    format(" \\~n"),
    format("\\"),
    maplist([X]>>format(" ~p", [X]), Xs),
    format(" /~n").


:- multifile group_element/2.
group_element(s(N), perm:E) :-
    phrase(indices(N), Indices),
    phrase(permutation(Indices), E).

:- multifile group_identity/2.
group_identity(s(N), perm:Is) :-
    phrase(indices(N), Is).

:- multifile group_inverse/3.
group_inverse(s(N), perm:X0, perm:X) :-
    group_element(s(N), perm:X0),
    reverse(X0, X).

:- multifile group_operation/4.
% P1 P2 x = P1 (P2 x)
group_operation(s(N), A, B, perm:ABI) :-
    phrase(indices(N), I),
    maplist(permutation_from_to(B), I, BI),
    maplist(permutation_from_to(A), BI, ABI).


indices(N) --> { zcompare(Ord, N, 0) }, indices_dispatch(Ord, N).
indices_dispatch(=, _) --> [].
indices_dispatch(>, N) --> { M #= N - 1 }, indices(M), [N].

permutation([]) --> [].
permutation(ABs) --> { select(A, ABs, Bs) }, [A], permutation(Bs).

permutation_from_to(perm:P, From, To) :- nth1(From, P, To).


permutation_cycle_decomposition(perm:P, Decomp) :-
    max_list(P, Front),
    phrase(work_front_current_perm_cycles(P, Front, Front, P), Decomp0),
    exclude([cycle:[_]]>>true, Decomp0, Decomp1),
    % Guarunteed to be disjoint, so cycles commute:
    sort(0, @>=, Decomp1, Decomp).

work_front_current_perm_cycles([], _Front, _Current, _Perm) --> [].
work_front_current_perm_cycles(Work0, Front, Current, Perm) -->
    { phrase(cycle_till_front(Front, Current, Perm, Work0, Work), Cycle) },
    [cycle:Cycle],
    (   { max_list(Work, NewFront) }
    ->  work_front_current_perm_cycles(Work, NewFront, NewFront, Perm)
    ;   []
    ).

cycle_till_front(Front, Current, Perm, Work0, Work) -->
    { permutation_from_to(perm:Perm, Current, Next) },
    [Current],
    { selectchk(Current, Work0, Work1) },
    cycle_till_front_follow(Front, Next, Perm, Work1, Work).

cycle_till_front_follow(Front, Front, _, Work, Work) --> !.
cycle_till_front_follow(Front, Next, Perm, Work0, Work) -->
    cycle_till_front(Front, Next, Perm, Work0, Work).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cycle_list_permutation(cycle:Cycle, List, Permutation) :-
    cycle_list_permutation_(Cycle, List, Permutation).
cycle_list_permutation_([], L, L).
cycle_list_permutation_([_], L, L).
cycle_list_permutation_([A, B | Rest], L, P) :-
    cycle_front_list_permutation([A, B | Rest], A, L, P0),
    maplist(unwrap_done, P0, P).

unwrap_done('$DONE'(X), X) :- !.
unwrap_done(X, X).

cycle_front_list_permutation([Final], Front, List, Permutation) :- !,
    selectchk(Final, List, '$DONE'(Front), Permutation).
cycle_front_list_permutation([From, To | Rest], Front, List0, Permutation) :-
    selectchk(From, List0, '$DONE'(To), List1),
    cycle_front_list_permutation([To | Rest], Front, List1, Permutation).


cycle_factorization(cycle:[First|Rest]) -->
    cycle_front_factorization(Rest, First).
cycle_front_factorization([], _) --> [].
cycle_front_factorization([A|Rest], First) -->
    [cycle:[First, A]],
    cycle_front_factorization(Rest, First).


test_cycle_factorization(Cycle, List) :-
    phrase(cycle_factorization(Cycle), Factors),
    foldl(cycle_list_permutation, Factors, List, Result1),
    cycle_list_permutation(Cycle, List, Result2),
    ( dif(Result1, Result2) -> throw(Result1\=Result2) ; true ).


cycle_composition_list_permutation(Cycles, List, Permutation) :-
    foldl(cycle_list_permutation, Cycles, List, Permutation).


cycle_composition_factorization([]) --> [].
cycle_composition_factorization([C|Cs]) -->
    cycle_factorization(C),
    cycle_composition_factorization(Cs).


portray(cycle:Xs) :-
    is_list(Xs),
    format("( "),
    maplist([X]>>format("~p ", [X]), Xs),
    format(")").


%simplify_transposition_product([]) --> [].
%simplify_transposition_product([cycle:[M,M]|Rest]) -->
%    !, {print(1)}, simplify_transposition_product(Rest).
%simplify_transposition_product([cycle:[M,X], cycle:[M,X]|Rest]) -->
%    !, {print(2)}, simplify_transposition_product(Rest).
%simplify_transposition_product([cycle:[M,X], cycle:[M,Y]|Rest]) -->
%    { dif(X, Y), dif(M, Y) }, !,
%    {print(3)},
%    [cycle:[X,Y]],
%    simplify_transposition_product([cycle:[M,X] | Rest]).
%simplify_transposition_product([cycle:[M,X], cycle:[X,Y]|Rest]) -->
%    { dif(M, X), dif(X, Y), dif(M, Y) }, !,
%    {print(4)},
%    [cycle:[X,Y]],
%    simplify_transposition_product([cycle:[M,Y] | Rest]).
%simplify_transposition_product([cycle:[M,X], cycle:[Y,Z]|Rest]) -->
%    { dif(M, Y), dif(X, Z), dif(M, Z) }, !,
%    {print(5)},
%    [cycle:[Y,Z]],
%    simplify_transposition_product([cycle:[M,X] | Rest]).
%simplify_transposition_product([Factor|Rest]) -->
%    {print(6)},
%    [Factor],
%    simplify_transposition_product(Rest).

