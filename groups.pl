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

:- op(300, xfy, x).

group_title((z(N), +), Title) :-
    format(string(Title), "the integers mod ~d under addition", [N]).
group_title((z(N), *), Title) :-
    format(string(Title), "the integers mod ~d under multiplication", [N]).
group_title((A x B, Op), Title) :-
    group_title((A, Op), ATitle),
    group_title((B, Op), BTitle),
    format(string(Title), "~s  x  ~s", [ATitle, BTitle]).
group_title(G, Title) :-
    G = group{member:M, ident:E, inv:Inv, op:Op},
    format(string(Title), "the set defined by ~p under ~p with identity ~p and inverse ~p", [M, Op, E, Inv]).

group_element(z(N), A) :- group_element((z(N), +), A).
group_element((z(N), +), A) :- 
    Hi is N - 1,
    A in 0..Hi.
group_element((z(N), *), A) :- 
    Hi is N - 1,
    A in 1..Hi.
group_element((G x H, Op), Gx-Hx) :-
    group_element((G, Op), Gx),
    group_element((H, Op), Hx).
group_element(group{member:Member, ident:_E, inv:_Inv, op:_Op}, A) :-
    call(Member, A).


group_inverse(z(N), A, AInv) :- group_inverse((z(N), +), A, AInv).
group_inverse((z(N), +), A, AInv) :-
    maplist(group_element((z(N), +)), [A, AInv]),
    AInv #= (N - A) mod N.
group_inverse((z(N), *), A, AInv) :-
    maplist(group_element((z(N), +)), [A, AInv]),
    (AInv * A) mod N #= 1.
group_inverse((G x H, Op), Gx-Hx, GxInv-HxInv) :-
    group_inverse((G, Op), Gx, GxInv),
    group_inverse((H, Op), Hx, HxInv).
group_inverse(group{member:_M, ident:_E, inv:Inv, op:_Op}, A, AInv) :-
    call(Inv, A, AInv).


group_identity(z(_), I) :- group_identity((z(_), +), I).
group_identity((z(_), +), 0).
group_identity((z(_), *), 1).
group_identity((G x H, Op), Gi-Hi) :-
    group_identity((G, Op), Gi),
    group_identity((H, Op), Hi).
group_identity(group{member:_M, ident:E, inv:_Inv, op:_Op}, E).


group_slice(_, []).
group_slice(G, [A | As]) :-
    group_element(G, A),
    group_slice(G, As).


group_operator(z(N), A, B, C) :- group_operator((z(N), +), A, B, C).
group_operator((z(N), +), A, B, C) :-
    maplist(group_element((z(N), +)), [A, B, C]),
    C #= (A + B) mod N.
group_operator((z(N), *), A, B, C) :-
    maplist(group_element((z(N), *)), [A, B, C]),
    C #= (A * B) mod N.
group_operator((G x H, Op), Gx-Hx, Gy-Hy, Gz-Hz) :-
    group_slice((G, Op), [Gx, Gy, Gz]),
    group_slice((H, Op), [Hx, Hy, Hz]),
    group_operator((G, Op), Gx, Gy, Gz),
    group_operator((H, Op), Hx, Hy, Hz).
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
