:- module(zn, [
    group_title/2,
    group_element/2,
    group_identity/2,
    group_inverse/3,
    group_operator/4
]).

:- use_module(library(clpfd)).
:- use_module(groups).

groups:group_title(z(N), Title) :-
    groups:group_title((z(N), +), Title).
groups:group_title((z(N), +), Title) :-
    format(string(Title), "the integers mod ~d under addition", [N]).
groups:group_title((z(N), *), Title) :-
    format(string(Title), "the integers mod ~d under multiplication", [N]).

groups:group_element(z(N), A) :- !, group_element__zn_add(A, N).
groups:group_element((z(N), +), A) :- !, group_element__zn_add(A, N).
groups:group_element((z(N), *), A) :- !, group_element__zn_mul(A, N).
group_element__zn_add(A, N) :- 
    Hi is N - 1,
    A in 0..Hi.
group_element__zn_mul(A, N) :-
    Hi is N - 1,
    A in 0..Hi.

groups:group_identity(z(_), I) :- !, group_identity((z(_), +), I).
groups:group_identity((z(_), +), 0) :- !.
groups:group_identity((z(_), *), 1) :- !.

groups:group_inverse(z(N), A, AInv) :- !, group_inverse__zn_add(A, AInv, N).
groups:group_inverse((z(N), +), A, AInv) :- !, group_inverse__zn_add(A, AInv, N).
groups:group_inverse((z(N), *), A, AInv) :- !, group_inverse__zn_mul(A, AInv, N).
group_inverse__zn_add(A, AInv, N) :-
    maplist(group_element((z(N), +)), [A, AInv]),
    AInv #= (N - A) mod N.
group_inverse__zn_mul(A, AInv, N) :-
    maplist(group_element((z(N), +)), [A, AInv]),
    (AInv * A) mod N #= 1.

% P1 P2 x = P1 (P2 x)
groups:group_operator(z(N), A, B, C) :- !, group_operator__zn_add(A, B, C, N).
groups:group_operator((z(N), +), A, B, C) :- !, group_operator__zn_add(A, B, C, N).
groups:group_operator((z(N), *), A, B, C) :- !, group_operator__zn_mul(A, B, C, N).
group_operator__zn_add(A, B, C, N) :-
    maplist(group_element((z(N), +)), [A, B, C]),
    C #= (A + B) mod N.
group_operator__zn_mul(A, B, C, N) :-
    maplist(group_element((z(N), *)), [A, B, C]),
    C #= (A * B) mod N.
