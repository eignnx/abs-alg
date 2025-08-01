:- module(matrix, [
    group_title/2,
    group_element/2,
    group_identity/2,
    group_inverse/3,
    group_operator/4
]).

:- encoding(utf8).

:- use_module(library(clpfd)).
:- use_module(groups).
:- use_module(fields, [
    field_element/2,
    field_add_ident/2,
    field_mul_ident/2,
    field_add_inv/3,
    field_mul_inv/3,
    field_add_op/4,
    field_mul_op/4,
    field_expr_eval/3
]).
:- use_module(utils).

groups:group_title(gl(N, z(P)), Title) :-
    format(string(Title), "the group of invertible ~dx~d matrices over the field Z_~d", [N, P]).

groups:group_element(gl(2, Field), M) :-
    Field = z(P), prime(P), !,
    maplist(fields:field_element(Field), [A, B, C, D]),
    M = m(r(A, B), r(C, D)),
    m2_det(Field, M, Det),
    field_add_ident(Field, Zero),
    dif(Det, Zero).

groups:group_identity(gl(2, Field), I) :-
    Field = z(P), prime(P), !,
    AI = 0, MI = 1,
    I = m(r(MI, AI), r(AI, MI)).

groups:group_inverse(gl(2, Field), M, MInv) :-
    Field = z(P), prime(P), !,
    M = m(r(A, B), r(C, D)),
    m2_det(Field, M, Det),
    field_expr_eval(Field, {1}/Det, DetInv),
    field_expr_eval(Field, DetInv *  D, W),
    field_expr_eval(Field, DetInv * -B, X),
    field_expr_eval(Field, DetInv * -C, Y),
    field_expr_eval(Field, DetInv *  A, Z),
    MInv = m(r(W, X), r(Y, Z)).

groups:group_operator(gl(2, F), M1, M2, M3) :-
    F = z(_), !,

    M1 = m(r(A1, B1),
           r(C1, D1)),
    M2 = m(r(A2, B2),
           r(C2, D2)),
    M3 = m(r(A3, B3),
           r(C3, D3)),

    field_expr_eval(F, A1*A2 + B1*C2, A3),
    field_expr_eval(F, C1*A2 + D1*C2, C3),
    field_expr_eval(F, A1*B2 + B1*D2, B3),
    field_expr_eval(F, C1*B2 + D1*D2, D3).

m2_det(z(P), M, Det) :-
    M = m(r(A, B), r(C, D)),
    field_expr_eval(z(P), A*D - B*C, Det).

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


user:portray(Matrix) :-
    m_nxn(Matrix, _),
    phrase(render(Matrix), Codes),
    format("~s", [Codes]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
render(Matrix) -->
    { functor(Matrix, _, N) },
    render_rows(1, N, Matrix).

render_rows(1, N, Matrix) --> !,
    { arg(1, Matrix, Row) },
    `\n⎡`, render_row(Row), `⎤\n`,
    render_rows(2, N, Matrix).
render_rows(N, N, Matrix) --> !,
    { arg(N, Matrix, Row) },
    `⎣`, render_row(Row), `⎦`.
render_rows(I0, N, Matrix) -->
    { arg(I0, Matrix, Row) },
    `⎢`, render_row(Row), `⎥\n`,
    { I is I0 + 1 },
    render_rows(I, N, Matrix).

render_row(Row) -->
    { Row =.. [r | Elements] },
    sequence(utils:portray, ` `, Elements).


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


