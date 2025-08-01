:- module(fields, [
    field_element/2,
    field_add_ident/2,
    field_mul_ident/2,
    field_add_inv/3,
    field_mul_inv/3,
    field_add_op/4,
    field_mul_op/4,
    field_expr_eval/3
]).

:- use_module(library(clpfd)).
:- use_module(utils).

:- multifile field_element/2.
:- multifile field_add_ident/2.
:- multifile field_mul_ident/2.
:- multifile field_add_inv/3.
:- multifile field_mul_inv/3.
:- multifile field_add_op/4.
:- multifile field_mul_op/4.

field_element(z(N), X) :-
    prime(N),
    M is N - 1,
    X in 0..M.

field_add_ident(z(_), 0).

field_mul_ident(z(_), 1).

field_add_inv(z(N), X, Y) :-
    M is N - 1,
    [X, Y] ins 0..M,
    field_add_op(z(N), X, Y, 0).

field_mul_inv(z(N), X, Y) :-
    M is N - 1,
    [X, Y] ins 0..M,
    field_mul_op(z(N), X, Y, 1).

field_add_op(z(N), A, B, C) :-
    C #= (A + B) mod N.

field_mul_op(z(N), A, B, C) :-
    M is N - 1,
    [A, B, C] ins 0..M,
    C #= (A * B) mod N.


field_expr_eval(F, Expr, Eval) :-
    expr_field_eval(Expr, F, Eval).

expr_field_eval(V, _, V) :- var(V), !.
expr_field_eval({0}, F, AI) :- !, field_add_ident(F, AI).
expr_field_eval({1}, F, MI) :- !, field_mul_ident(F, MI).
expr_field_eval(-A0, F, B) :- !,
    expr_field_eval(A0, F, A),
    field_add_inv(F, A, B).
expr_field_eval(A0 ^ -1, F, B) :- !,
    expr_field_eval(A0, F, A),
    field_mul_inv(F, A, B).
expr_field_eval(A0 + B0, F, C) :- !,
    expr_field_eval(A0, F, A),
    expr_field_eval(B0, F, B),
    field_add_op(F, A, B, C).
expr_field_eval(A0 * B0, F, C) :- !,
    expr_field_eval(A0, F, A),
    expr_field_eval(B0, F, B),
    field_mul_op(F, A, B, C).
expr_field_eval(A0 - B0, F, C) :- !,
    expr_field_eval(A0, F, A),
    expr_field_eval(B0, F, B),
    field_add_inv(F, B, BInv),
    field_add_op(F, A, BInv, C).
expr_field_eval(A0 / B0, F, C) :- !,
    expr_field_eval(A0, F, A),
    expr_field_eval(B0, F, B),
    field_mul_inv(F, B, BInv),
    field_mul_op(F, A, BInv, C).
expr_field_eval(Element, _, Element).

% ?- prime(N), forall((field_element(z(N), X), field_mul_inv(z(N), X, Y), label([X,Y])), format("~p^-1 = ~p~n", [X, Y])).
