:- module(permute, [
    group_title/2,
    group_element/2,
    group_identity/2,
    group_inverse/3,
    group_operator/4
]).

:- encoding(utf8).

:- use_module(library(clpfd)).
:- use_module(groups).


cycle(cycle:[]).
cycle(cycle:[X | Xs]) :- X in 1..sup, cycle(cycle:Xs).

% perm:[2, 4, 3, 1] maps 1->2, 2->4, 3->3, 4->1.
is_permutation(perm:Xs) :-
    phrase(indices(_), Is),
    phrase(permutation(Is), Xs).

groups:group_title(s(N), Title) :-
    format(string(Title), "the symmetric group on sets of ~d elements", [N]).

user:portray(perm:[X|Xs]) :-
    length([X|Xs], N),
    phrase(indices(N), [I|Is]),
    format("~n⎛"),
    format("~p", [I]),
    maplist([X]>>format(" ~p", [X]), Is),
    format("⎞~n"),
    format("⎝"),
    format("~p", [X]),
    maplist([X]>>format(" ~p", [X]), Xs),
    format("⎠"),
    flush_output(current_output).

user:portray(cycle:Xs) :-
    is_list(Xs),
    format("( "),
    maplist([X]>>format("~p ", [X]), Xs),
    format(")"),
    flush_output(current_output).



groups:group_element(s(N), E) :- !, group_element_(E, N).
group_element_(perm:E, N) :-
    phrase(indices(N), Indices),
    phrase(permutation(Indices), E).

groups:group_identity(s(N), I) :- !, group_identity_(I, N).
group_identity_(perm:Is, N) :-
    phrase(indices(N), Is).

groups:group_inverse(s(N), X0, X) :- !, group_inverse_(X0, X, N).
group_inverse_(perm:X0, perm:X, N) :-
    %freeze(X0, group_element_(perm:X0, N)),
    phrase(indices(N), Is),
    maplist([A, I, A-I]>>true, X0, Is, XsIs),
    keysort(XsIs, XsIsSorted),
    maplist([A-I, I]>>true, XsIsSorted, X).

% P1 P2 x = P1 (P2 x)
groups:group_operator(s(N), A, B, C) :- !, group_operator_(A, B, C, N).
group_operator_(A, B, perm:ABI, N) :-
    %freeze(A, group_element_(A, N)),
    %freeze(B, group_element_(B, N)),
    %freeze(ABI, group_element_(perm:ABI, N)),
    phrase(indices(N), I),
    maplist(permutation_from_to(B), I, BI),
    maplist(permutation_from_to(A), BI, ABI).


indices(N) --> { N in 0..sup, zcompare(Ord, N, 0) }, indices_dispatch(Ord, N).
indices_dispatch(=, _) --> [].
indices_dispatch(>, N) --> { M #= N - 1 }, indices(M), [N].

permutation([]) --> [].
permutation(ABs) --> [A], { select(A, ABs, Bs) }, permutation(Bs).

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
    [swap(First, A)],
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

