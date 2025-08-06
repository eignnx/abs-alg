:- module(sylow, [
    sylow//1
]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- use_module(utils).

factorization(1, []) :- !.
factorization(N, [N]) :- prime(N), !.
factorization(N, [Factor|Factors]) :-
    floor_sqrt(N, NSqrt),
    between(2, NSqrt, Factor),
    divides(Factor, N),
    prime(Factor),
    !,
    M is N div Factor,
    factorization(M, Factors).

floor_sqrt(N, M) :- nth_integer_root_and_remainder(2, N, M, _).
divides(A, B) :- 0 is B mod A.

collect_powers([X|Zs]) --> collect_powers(Zs, 1, X).
collect_powers([], Count, X) --> !, [X^Count].
collect_powers([X|Zs], Count0, X) --> !,
    { Count is Count0 + 1 },
    collect_powers(Zs, Count, X).
collect_powers([Y|Zs], Count, X) -->
    [X^Count],
    collect_powers(Zs, 1, Y).


sylow1(GroupOrder) -->
    { factorization(GroupOrder, Factors) },
    { phrase(collect_powers(Factors), FactorsPow) },
    sylow1_(FactorsPow).
sylow1_([]) --> [].
sylow1_([P^R | Rest]) -->
    foreach(between(1, R, I), pow(P, I)),
    sylow1_(Rest).

pow(P, I) --> { Q is P^I }, [order(_) = Q].


sylow2(GroupOrder) -->
    { factorization(GroupOrder, Factors) },
    { phrase(collect_powers(Factors), FactorsPow) },
    sylow2_(FactorsPow, GroupOrder).
sylow2_([], _) --> [].
sylow2_([P^R | Rest], GroupOrder) -->
    { PR is P^R },
    { setof(I, (between(1, GroupOrder, I), 1 is I mod P), Cong) },
    { setof(J, (between(1, GroupOrder, J), divides(J, GroupOrder)), Factors) },
    { intersection(Cong, Factors, Possibilities) },
    [count(subgroup_order(PR)) in Possibilities],
    sylow2_(Rest, GroupOrder).

sylow(GroupOrder) -->
    { prime(GroupOrder) }, !,
    format_("A group `G` of prime order ~d has no proper subgroups.", [GroupOrder]).
sylow(GroupOrder) -->
    format_("A group `G` of order ~d has subgroups of orders ", [GroupOrder]),
    { phrase(sylow1(GroupOrder), Subgroups) },
    english_sequence(``, describe_subgroup, `and`, Subgroups), `.\n`,
    format_("`G` has:~n", []),
    { phrase(sylow2(GroupOrder), PSubgroups) },
    sequence(describe_p_subgroup, PSubgroups).

describe_subgroup(order(_) = N) --> integer(N).

describe_p_subgroup(count(subgroup_order(I)) in [1]) --> !,
    format_("~t~4|- a single normal subgroup of order ~d~n", [I]).
describe_p_subgroup(count(subgroup_order(I)) in [N]) --> !,
    format_("~t~4|- exactly ~d order-~d subgroups~n", [N, I]).
describe_p_subgroup(count(subgroup_order(I)) in [N|Ns]) -->
    format_("~t~4|- "),
    english_sequence(`either`, integer, `or`, [N|Ns]),
    format_(" order-~d subgroups~n", [I]).


english_sequence(_Coord, Nt, _Conj, [Item]) --> !, call(Nt, Item).
english_sequence(``, Nt, Conj, Items) --> !,
    english_sequence_(Items, Nt, Conj).
english_sequence(Coord, Nt, Conj, Items) -->
    Coord, ` `, english_sequence_(Items, Nt, Conj).

english_sequence_([X, Y, Z], Nt, Conj) --> !,
    call(Nt, X), `, `,
    call(Nt, Y), `, `,
    Conj, ` `,
    call(Nt, Z).
english_sequence_([X, Y], Nt, Conj) --> !,
    call(Nt, X), ` `, Conj, ` `, call(Nt, Y).
english_sequence_([X | Rest], Nt, Conj) -->
    call(Nt, X), `, `, english_sequence_(Rest, Nt, Conj).
