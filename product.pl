:- module(product, [
    group_title/2,
    group_element/2,
    group_identity/2,
    group_inverse/3,
    group_operator/4
]).

:- use_module(library(clpfd)).
:- use_module(groups).

:- op(300, xfy, x).

groups:group_title(A x B, Title) :-
    group_title(A, ATitle),
    group_title(B, BTitle),
    format(string(Title), "~s  x  ~s", [ATitle, BTitle]).
groups:group_element(G x H, Gx-Hx) :-
    !,
    group_element(G, Gx),
    group_element(H, Hx).
groups:group_inverse(G x H, Gx-Hx, GxInv-HxInv) :-
    !,
    group_inverse(G, Gx, GxInv),
    group_inverse(H, Hx, HxInv).
groups:group_identity(G x H, Gi-Hi) :-
    !,
    group_identity(G, Gi),
    group_identity(H, Hi).
groups:group_operator(G x H, Gx-Hx, Gy-Hy, Gz-Hz) :-
    !,
    group_slice(G, [Gx, Gy, Gz]),
    group_slice(H, [Hx, Hy, Hz]),
    group_operator(G, Gx, Gy, Gz),
    group_operator(H, Hx, Hy, Hz).

