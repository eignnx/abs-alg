:- module(utils, [
    op(50, xfy, inst),
    inst/2,
    prime/1,
    portray//1,
    html//1,
    tag//2,
    tag//3,
    tr//1,
    td//1,
    th//1,
    bijective_numeral//1
]).

:- encoding(utf8).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- op(50, xfy, inst).

Goal inst X :-
    call(Goal),
    ( var(X) ->
        catch(label([X]), _, true)
    ;
        term_variables(X, Vs), label(Vs)
    ).

prime(N) :-
    length([_, _ | _], N),
    nth_integer_root_and_remainder(2, N, M, _),
    \+ (
        between(2, M, I),
        0 is N mod I
    ).

portray(X) -->
    { format(codes(Codes), "~p", [X]) }, 
    Codes.

html(Nodes) -->
    `<html>`,
    sequence(callit, Nodes),
    `</html>`.

callit(Goal) --> Goal.

tag(Tag, Children) -->
    `<`, atom(Tag), `>`,
        sequence(callit, Children),
    `</`, atom(Tag), `>`.
tag(Tag, Attrs, Children) -->
    `<`, atom(Tag), ` `,
        sequence(attr, ` `, Attrs),
    `>`,
        sequence(callit, Children),
    `</`, atom(Tag), `>`.

attr(Key=Value) --> atom(Key), `="`, Value, `"`.

tr(Children) --> tag(tr, Children).
td(Children) --> tag(td, Children).
th(Children) --> tag(th, Children).


bijective_numeral(N) -->
    { zcompare(Ord, N, 0) },
    bijective_numeral_(Ord, N).
bijective_numeral_(=, 0) --> [].
bijective_numeral_(>, N) -->
    { alphabet(greek, Alphabet, Base) },
    { LetterIndex is (N - 1) mod Base },
    { nth0(LetterIndex, Alphabet, Letter) },
    { Next is (N - 1) div Base },
    bijective_numeral(Next),
    [Letter].
bijective_numeral_(<, N) -->
    { throw(error(type_error(bijective_numeral_expected_positive(N)), _)) }.

alphabet(latin, `ABCDEFGHIJKLMNOPQRSTUVWXYZ`, 26).
alphabet(greek, `ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ`, 24).
