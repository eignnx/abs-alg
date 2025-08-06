:- module(cycle_graph, [
    group_cycles/2,
    cayley_table_ordering//1
]).

:- use_module(library(rbtrees)).
:- use_module(library(dcg/high_order)).

:- use_module(groups).
:- use_module(utils).


group_cycles(G, Cycles) :-
    setof(E, group_element(G, E) inst E, Elements),
    phrase(all_cycles(G, Elements), CyclesDup),
    setof(X, member(X, CyclesDup), Cycles).

cycle_graph(G) :-
    group_cycles(G, Cycles),
    %[First|Cycles] = Cycles0,
    %( First \= Elements -> print("first \\= elements"), format("~k~n", [First]) ; true ),
    maplist([C]>>format("~p~n", [C]), Cycles),
    format("-------------------------------------~n"),
    phrase(cayley_table_ordering_from_cycles(Cycles), Ordering),
    maplist([C]>>format("~p~n", [C]), Ordering).


cycle(G, Element) -->
    { group_operator(G, Element, Element, Acc) },
    [Element],
    cycle(G, Element, Acc).
cycle(G, _Element, Ident) -->
    { group_identity(G, Ident) }, !, [Ident].
cycle(G, Element, Acc0) -->
    { group_operator(G, Element, Acc0, Acc) },
    [Acc0],
    cycle(G, Element, Acc).


all_cycles(_G, []) --> !, [].
all_cycles(G, Unvisited0) -->
    { selectchk(X, Unvisited0, Unvisited) },
    { phrase(cycle(G, X), Cycle0) },
    ( { simple_cycle(Cycle0) } -> [] ;
        { sort(Cycle0, Cycle) },
        [Cycle]
    ),
    all_cycles(G, Unvisited).


simple_cycle([]).
%simple_cycle([_]).
%simple_cycle([_, _]).


gen_graph(G, Cycles) -->
    `graph G {`,
    gen_cycles(G, Cycles),
    `}`.


gen_cycles(_, _) --> [].


cayley_table_ordering(G) -->
    { setof(E, group_element(G, E) inst E, Elements) },
    { phrase(all_cycles(G, Elements), CyclesDup) },
    { setof(X, member(X, CyclesDup), Cycles) },
    cayley_table_ordering_from_cycles(Cycles).

cayley_table_ordering_from_cycles(Cycles) -->
    { append(Cycles, AllUnsorted) },
    { msort(AllUnsorted, All) },
    { clumped(All, Tallies) },
    { maplist([X-N, N-X]>>true, Tallies, CountsElements) },
    { msort(CountsElements, CountsElementsSortedRev) },
    { reverse(CountsElementsSortedRev, CountsElementsSorted) },
    sequence(no_count, CountsElementsSorted).

no_count(_N-X) --> [X].



