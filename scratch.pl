:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- encoding(utf8).

:- use_module(library(clpfd)).
:- use_module(library(rbtrees)).

:- use_module(groups).
:- use_module(permute).
:- use_module(zn).
:- use_module(product).

caley(Group, RbElementsIndices, Ax-A, Bx-B, Cx-C) :-
    rb_in(A, Ax, RbElementsIndices),
    rb_in(B, Bx, RbElementsIndices),
    group_operator(Group, A, B, C),
    rb_lookup(C, Cx, RbElementsIndices).

main(Group) :-
    format("Collecting group elements...~n"),
    setof(X, (
        group_element(Group, X),
        (var(X) -> label([X]) ; true)
    ), Elements),
    format("Collected "),
    length(Elements, N),
    format("~d elements.~n", [N]),
    phrase(permute:indices(N), Is),

    format("Building index mappings...~n"),
    pairs_keys_values(Pairs, Is, Elements),
    pairs_keys_values(ElementsIndices, Elements, Is),
    list_to_rbtree(ElementsIndices, RbElementsIndices),
    format("Built.~n"),

    format("Building Cayley table...~n"),
    bagof(
        (Ax*Bx)-(A*B=Cx-C),
        caley(Group, RbElementsIndices, Ax-A, Bx-B, Cx-C),
        TableAsList
    ),
    format("Built.~n"),
    format("Converting table to search tree...~n"),
    list_to_rbtree(TableAsList, Table),
    format("Converted.~n"),

    format("Opening output file...~n"),
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
    ),
    format("Finished.~n").

page(Group, Table, Pairs) -->
    { group_title(Group, GroupTitleStr), string_codes(GroupTitleStr, GroupTitle) },
    html([
        tag(head, [
            tag(meta, [charset=`UTF-8`], []),
            tag(link, [href=`style.css`, rel=`stylesheet`], [])
        ]),
        tag(body, [

            tag(section, [id=`container`], [

                { format("Rendering legend...~n") },
                tag(table, [
                    tag(caption, [`Legend`]),
                    tag(thead, [
                        tr([
                            th([`Abbreviation`]),
                            th([`Element`])
                        ])
                    ]),
                    tag(tbody, [
                        sequence(legend_row, Pairs)
                    ])
                ]),

                { format("Rendering Cayley table...~n") },
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
        ])
    ]).

legend_row(Idx-Val) -->
    tr([
        tag(th, [scope=`row`], [idx_repr(Idx)]),
        td([tag(pre, [portray(Val)])])
    ]).

header_columns(Pairs) -->
    sequence(column_header, `\n`, Pairs).

:- det(column_header//1).
column_header(Idx-Val) -->
    tag(th, [title=portray(Val)], [idx_repr(Idx)]).

table_body(Table, Pairs) -->
    { length(Pairs, Order) },
    sequence(body_row(Table, Pairs, Order), Pairs).

body_row(Table, Pairs, Order, Idx-Val) -->
    { format('Rendering row ~d of ~d...~n', [Idx, Order]) },
    tr([
        row_header(Idx, Val),
        sequence(table_cell(Table, Order, Idx-Val), Pairs)
    ]).

:- det(row_header//2).
row_header(Idx, Val) -->
    tag(th, [scope=`row`, title=portray(Val)], [idx_repr(Idx)]).

:- det(table_cell//4).
table_cell(Table, Order, RowIdx-_RowVal, ColIdx-_ColVal) -->
    { rb_lookup((RowIdx*ColIdx), _*_=(ResultIdx-Result), Table) },
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
