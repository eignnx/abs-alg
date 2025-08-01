:- module(groups, [
    group_title/2,
    group_element/2,
    group_identity/2,
    group_inverse/3,
    group_operator/4,
    group_slice/2,
    group_element_power/4
]).

:- multifile group_title/2.
:- multifile group_element/2.
:- multifile group_identity/2.
:- multifile group_inverse/3.
:- multifile group_operator/4.

:- use_module(library(clpfd)).

group_title(G, Title) :-
    G = group{member:M, ident:E, inv:Inv, op:Op},
    format(string(Title), "the set defined by ~p under ~p with identity ~p and inverse ~p", [M, Op, E, Inv]).

group_element(group{member:Member, ident:_E, inv:_Inv, op:_Op}, A) :-
    call(Member, A).


group_inverse(group{member:_M, ident:_E, inv:Inv, op:_Op}, A, AInv) :-
    call(Inv, A, AInv).


group_identity(group{member:_M, ident:E, inv:_Inv, op:_Op}, E).


group_slice(_, []).
group_slice(G, [A | As]) :-
    group_element(G, A),
    group_slice(G, As).


group_operator(G, A, B, C) :-
    G = group{member:_M, ident:_E, inv:_Inv, op:Op},
    call(Op, A, B, C).


group_element_power(G, A, K, B) :-
    zcompare(Ord, K, 0),
    ord_group_element_power(Ord, G, A, K, B).
ord_group_element_power(=, G, _, _, B) :-
    group_identity(G, B).
ord_group_element_power(<, G, A, K, B) :-
    group_inverse(G, A, AInv),
    AbsK #= abs(K),
    ord_group_element_power(>, G, AInv, AbsK, B).
ord_group_element_power(>, G, A, K, B) :-
    length(As, K),
    maplist(=(A), As),
    group_identity(G, E),
    foldl(group_operator(G), As, E, B).


group_expr_eval(G, Expr, Eval) :-
    expr_group_eval(Expr, G, Eval).

expr_group_eval(X0*Y0, G, Z) :-
    !,
    expr_group_eval(X0, G, X),
    expr_group_eval(Y0, G, Y),
    group_operator(G, X, Y, Z).
expr_group_eval(i(X0), G, Y) :-
    !,
    expr_group_eval(X0, G, X),
    group_inverse(G, X, Y).
expr_group_eval(e, G, Ident) :-
    !,
    group_identity(G, Ident).
expr_group_eval(X^N, G, Y) :-
    !,
    group_element_power(G, X, N, Y).
expr_group_eval(X, G, X) :-
    group_element(G, X).
