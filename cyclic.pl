:- use_module(groups, [
    group_title/2,
    group_element/2,
    group_identity/2,
    group_inverse/3,
    group_operator/4,
    group_slice/2,
    group_element_power/4
]).
:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- use_module(utils).

group_generator_elements(G, Gen, [Gen|Els]) :-
    group_element(G, Gen),
    group_identity(G, Ident),
    group_ident_generator_accumulator_elements_(G, Ident, Gen, Gen, Els).

group_ident_generator_accumulator_elements_(G, Ident, Gen, Acc0, Els) :-
    if_(Acc0 = Ident,
        Els = [],
        (
            group_operator(G, Gen, Acc0, Acc),
            Els = [Acc|Els0],
            group_ident_generator_accumulator_elements_(G, Ident, Gen, Acc, Els0)
        )
    ).


group(G) :- phrase(group_counterexamples(G), _).

group_counterexamples(G) -->
    { group_identity(G, I) inst I },

    % Operation closure:
    ( { disprove_operation_closure(G, A1, B1, C1) } ->
        [operation_not_closed(A1 * B1 = C1)]
    ;
        []
    ),

    % Identity element:
    ( { group_element(G, X1) inst X1, group_operator(G, I, X1, Y1), dif(Y1, X1) } ->
        [identity_property_doesnt_hold(I*X1 = Y1)]
    ;
        []
    ),

    % Inverses:
    (   { group_element(G, X) inst X },
        { group_inverse(G, X, Y) },
        { group_element(G, Y) inst Y },
        { group_operator(G, Y, X, Z) inst Z },
        { dif(Z, I) } ->
        [inverse_property_doesnt_hold(X*Y = Z)]
    ;
        []
    ),

    % Associativity:
    ( { disprove_associativity(G, A, B, C, ABC1, ABC2) } ->
        [non_associative_operator((A * B) * C = ABC1, A * (B * C) = ABC2)]
    ;
        []
    ).

disprove_operation_closure(G, A, B, C) :-
    group_element(G, A),
    group_element(G, B),
    group_operator(G, A, B, C) inst C,
    \+ group_element(G, C).

disprove_associativity(G, A, B, C, AB_C, A_BC) :-
    group_element(G, A) inst A,
    group_element(G, B) inst B,
    group_element(G, C) inst C,
    group_operator(G, A, B, AB),    group_element(G, AB),
    group_operator(G, AB, C, AB_C), group_element(G, AB_C),
    group_operator(G, B, C, BC),    group_element(G, BC),
    group_operator(G, A, BC, A_BC), group_element(G, A_BC),
    true inst [AB_C, A_BC],
    dif(AB_C, A_BC).


wrap(Goal, Reason) -->
    { phrase(Goal, Xs) },
    ( { Xs = [_|_] } -> { Wrapped =.. [Reason, Xs] }, [Wrapped] ; [] ).


group_subgroup(G, H) :- phrase(group_subgroup_counterexamples(G, H), _).

group_subgroup_counterexamples(G, H) -->
    wrap(group_counterexamples(G), supergroup_not_a_group),
    wrap(group_counterexamples(H), subgroup_not_a_group),

    % H's set is a subset of G's set:
    ( { group_element(H, Hx) inst Hx, \+ group_element(G, Hx) } ->
        [supergroup_does_not_contain_subgroup_element(Hx)]
    ;
        []
    ),

    % Closure under induced operation:
    (   { group_element(H, Hx) inst Hx },
        { group_element(H, Hy) inst Hy },
        { group_operator(G, Hx, Hy, Hz) },
        { \+group_element(H, Hz) } ->
        [induced_op_not_closed_for(Hx, Hy, Hz)]
    ;
        []
    ).

inverse_closed(G, X) :-
    group_inverse(G, X, XInv),
    group_element(G, XInv).

operator_closed(G, X, Y) :-
    group_operator(G, X, Y, Z),
    group_element(G, Z).


group_subgroup_left_cosets(G, H, Cosets) :-
    setof(
        Coset,
        Gx^(
            group_element(G, Gx),
            setof(
                C,
                Hx^(
                    group_element(H, Hx),
                    group_operator(G, Gx, Hx, C) inst C
                ),
                Coset
            )
        ),
        Cosets
    ).

group_subset_subgroup(G, Member, group{member: Member, ident: E, inv: Inv, op: Op}) :-
    group_identity(G, E),
    Inv = group_inverse(G),
    Op = group_operator(G).

