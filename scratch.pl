:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- encoding(utf8).

:- use_module(library(clpfd)).

:- use_module(groups).
:- use_module(permute).
:- use_module(zn).
:- use_module(product).

caley(Group, Pairs, Ax-A, Bx-B, Cx-C) :-
    member(Ax-A, Pairs),
    member(Bx-B, Pairs),
    group_operator(Group, A, B, C),
    member(Cx-C, Pairs),
    format("#~p * #~p = #~p~n", [Ax, Bx, Cx]).

main(Group) :-
    setof(X, (
        group_element(Group, X),
        (var(X) -> label([X]) ; true)
    ), Elements),
    maplist([X]>>format("~p~n", [X]), Elements),
    format("~`-t~40|~n"),
    length(Elements, N),
    phrase(permute:indices(N), Is),
    pairs_keys_values(Pairs, Is, Elements),

    bagof(
        (Ax-A)*(Bx-B)=(Cx-C),
        caley(Group, Pairs, Ax-A, Bx-B, Cx-C),
        Table
    ),

    setup_call_cleanup(
        open('table.html', write, Stream, [encoding(utf8)]),
        (
            format("Generating HTML table...~n"),
            phrase(page(Group, Table, Pairs), Codes),
            format("Writing to file...~n"),
            format(Stream, "~s~n", [Codes])
        ),
        (
            format("Closing stream...~n"),
            close(Stream)
        )
    ).

page(Group, Table, Pairs) -->
    { group_title(Group, GroupTitleStr), string_codes(GroupTitleStr, GroupTitle) },
    html([
        tag(head, [
            tag(meta, [charset=`UTF-8`], []),
            tag(link, [href=`style.css`, rel=`stylesheet`], [])
        ]),
        tag(body, [
            tag(table, [
                tag(caption, [string(GroupTitle)]),
                tag(thead, [
                    tr([
                        th([`⋅`]), % For row headers below
                        header_columns(Pairs)
                    ])
                ]),
                tag(tbody, [
                    table_body(Table, Pairs)
                ])
            ])
        ])
    ]).

header_columns(Pairs) -->
    sequence(column_header, `\n`, Pairs).

:- det(column_header//1).
column_header(Idx-Val) -->
    tag(th, [title=portray(Val)], [idx_repr(Idx)]).

table_body(Table, Pairs) -->
    sequence(body_row(Table, Pairs), Pairs).

body_row(Table, Pairs, Idx-Val) -->
    tr([
        row_header(Idx, Val),
        sequence(table_cell(Table, Pairs, Idx-Val), Pairs)
    ]).

:- det(row_header//2).
row_header(Idx, Val) -->
    tag(th, [scope=`row`, title=portray(Val)], [idx_repr(Idx)]).
    
:- det(table_cell//4).
table_cell(Table, Pairs, RowIdx-_RowVal, ColIdx-_ColVal) -->
    { memberchk((RowIdx-_)*(ColIdx-_)=(ResultIdx-Result), Table) },
    { length(Pairs, Order) },
    { Hue is 360 * 9/10 * (ResultIdx - 1) / Order + 180 },
    { format(codes(Style), "background-color: oklch(0.6888 0.1629 ~p);", [Hue]) },
    tag(td, [
        title=portray(Result),
        style=Style
    ], [idx_repr(ResultIdx)]).

idx_repr(N) --> bijective_numeral(N).
%idx_repr(N) --> number(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    { LetterIndex is (N - 1) mod 26 },
    { nth0(LetterIndex, `ABCDEFGHIJKLMNOPQRSTUVWXYZ`, Letter) },
    [Letter],
    { Next is (N - 1) div 26 },
    bijective_numeral(Next).
bijective_numeral_(<, N) -->
    { throw(error(type_error(bijective_numeral_expected_positive(N)), _)) }.
