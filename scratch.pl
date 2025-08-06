:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- encoding(utf8).

:- use_module(library(clpfd)).
:- use_module(library(rbtrees)).

:- use_module(groups).
:- use_module(permute).
:- use_module(zn).
:- use_module(matrix).
:- use_module(product).
:- use_module(cycle_graph).
:- use_module(sylow).
:- use_module(utils).

caley(Group, RbElementsIndices, Ax-A, Bx-B, Cx-C) :-
    rb_in(A, Ax, RbElementsIndices),
    rb_in(B, Bx, RbElementsIndices),
    group_operator(Group, A, B, C),
    rb_lookup(C, Cx, RbElementsIndices).

main(Group) :-
    format("Collecting group elements...~n"),
    phrase(cayley_table_ordering(Group), Elements),
    %setof(X, (
    %    group_element(Group, X) inst X
    %), Elements),
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
    { format("Finding cycles...~n") },
    { group_cycles(Group, Cycles) },
    { format("Found.~n") },
    { length(Pairs, Order) },
    { group_title(Group, GroupTitleStr), string_codes(GroupTitleStr, GroupTitle) },
    html([
        tag(head, [
            tag(meta, [charset=`UTF-8`], []),
            tag(link, [href=`style.css`, rel=`stylesheet`], []),
            tag(script, [src=`script.js`, defer], [])
        ]),

        tag(body, [
            tag(section, [
                { phrase(sylow(Order), Text) },
                { text_lines(Text, Lines) },
                sequence([Line]>>tag(p, [Line]), Lines)
            ]),
            tag(section, [
                sequence(render_cycle(Pairs), Cycles)
            ]),

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

render_cycle(Pairs, Cycle) -->
    tag(div, [class=`cycle`], [
        tag(label, [
            tag(input, [type=`checkbox`], []),
            `{`, sequence(render_cycle_element(Pairs), `, `, Cycle), `}`,
            { length(Cycle, CycleLen) },
            tag(span, [` size `, portray(CycleLen)])
        ])
    ]).

render_cycle_element(Pairs, Element) -->
    { memberchk(Index-Element, Pairs) },
    tag(data, [
        class=`element-portrayal`,
        title=portray(Element),
        value=portray(Index)
    ], [idx_repr(Index)]).
    %tag(pre, [class=`element-portrayal`], [portray(Element)]).

legend_row(Idx-Val) -->
    tr([
        tag(th, [scope=`row`], [idx_repr(Idx)]),
        td([tag(pre, [class=`element-portrayal`], [portray(Val)])])
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
    { format(codes(Style), "background-color: oklch(0.7715 0.1241 ~p);", [Hue]) },
    tag(td, [
        title=portray(Result),
        style=Style
    ], [
        tag(data, [value=portray(ResultIdx)], [idx_repr(ResultIdx)])
    ]).

idx_repr(N) --> bijective_numeral(N).
%idx_repr(N) --> number(N).

