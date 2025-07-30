:- use_module(library(clpfd)).

prime(N) :-
    length([_, _ | _], N),
    nth_integer_root_and_remainder(2, N, M, _),
    \+ (
        between(2, M, I),
        0 is N mod I
    ).

ring_element(z, X) :- X in inf..sup.
ring_element(z(N), X) :-
    prime(N),
    M is N - 1,
    X in 0..M.

ring_additive_ident(q, 0).
ring_additive_ident(z(_), 0).

ring_multiplicative_ident(q, 1).
ring_multiplicative_ident(z(_), 1).

ring_addition(q, A, B, C) :- C #= A + B.
ring_addition(z(N), A, B, C) :-
    C #= (A + B) mod N.

ring_additive_inverse(q, X, Y) :- Y #= -X.
ring_additive_inverse(z(N), X, Y) :-
    M is N - 1,
    [X, Y] ins 0..M,
    ring_addition(z(N), X, Y, 0).

ring_multiplication(q, A, B, C) :- C #= A * B.
ring_multiplication(z(N), A, B, C) :-
    M is N - 1,
    [A, B, C] ins 0..M,
    C #= (A * B) mod N.

ring_multiplicative_inverse(q, X, Y) :-
    Y is 1 / X.
ring_multiplicative_inverse(z(N), X, Y) :-
    M is N - 1,
    [X, Y] ins 0..M,
    ring_multiplication(z(N), X, Y, 1).

%ring_expr_eval(R, Expr, Eval) :-
    %expr_ring_eval(Expr, R, Eval).
%expr_ring_eval(

% ?- prime(N), forall((ring_element(z(N), X), ring_multiplicative_inverse(z(N), X, Y), label([X,Y])), format("~p^-1 = ~p~n", [X, Y])).
