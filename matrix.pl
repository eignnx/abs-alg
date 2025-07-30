:- use_module(library(clpfd)).

identity(N, I) :-
    length(Rows, N),
    indexed_maplist({N}/[J, Row]>>(
        ident_row(N, J, RowList),
        Row =.. [r | RowList]
    ), Rows),
    I =.. [m | Rows].


ident_row(N, J, Row) :-
    length(Row, N),
    indexed_maplist(
        {J}/[Idx, X]>>(Idx = J -> X = 1 ; X = 0),
        Row
    ).


indexed_maplist(Pred, Xs) :-
    indexed_maplist_(Xs, 0, Pred).
indexed_maplist_([], _, _Pred).
indexed_maplist_([X|Xs], Idx0, Pred) :-
    call(Pred, Idx0, X),
    Idx #= Idx0 + 1,
    indexed_maplist_(Xs, Idx, Pred).


m_nxn(Matrix, N) :-
    (var(Matrix) -> true ; functor(Matrix, m, N) ),
    length(Rows, N),
    maplist({N}/[Row]>>functor(Row, r, N), Rows),
    Matrix =.. [m | Rows].


:- multifile portray/1.
portray(Matrix) :-
    m_nxn(Matrix, N),
    format("~n"),
    forall(
        between(1, N, R),
        (
            ( between(0, 1, N) -> format("[")
            ; R = 1 -> format("/")
            ; R = N -> format("\\")
            ; true  -> format("|")
            ),
            arg(R, Matrix, Row),
            forall(
                between(1, N, C),
                (
                    arg(C, Row, Entry),
                    format(" ~p", [Entry])
                )
            ),
            ( between(0, 1, N) -> format(" ]~n")
            ; R = 1 -> format(" \\~n")
            ; R = N -> format(" /~n")
            ; true  -> format(" |~n")
            )

        )
    ).

m1_m2_product(M1, M2, M3) :-
    M1 = m(
        r(A1, B1),
        r(C1, D1)
    ),
    M2 = m(
        r(A2, B2),
        r(C2, D2)
    ),
    M3 = m(
        r(A3, B3),
        r(C3, D3)
    ),

    A3 #= A1*A2 + B1*C2,
    C3 #= C1*A2 + D1*C2,
    B3 #= A1*B2 + B1*D2,
    D3 #= C1*B2 + D1*D2.

m_det(M, Det) :-
    M = m(
        r(A, B),
        r(C, D)
    ),
    Det #= A*D - B*C.


