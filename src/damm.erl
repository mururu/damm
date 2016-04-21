-module(damm).

-export([checksum/1, encode/1, is_valid/1]).

-define(TABLE, {{0, 3, 1, 7, 5, 9, 8, 6, 4, 2},
                {7, 0, 9, 2, 1, 5, 4, 8, 6, 3},
                {4, 2, 0, 6, 8, 7, 1, 3, 5, 9},
                {1, 7, 5, 0, 9, 8, 3, 4, 2, 6},
                {6, 1, 2, 3, 0, 4, 5, 9, 7, 8},
                {3, 6, 7, 4, 2, 0, 9, 5, 8, 1},
                {5, 8, 6, 9, 7, 2, 0, 1, 3, 4},
                {8, 9, 4, 5, 3, 6, 2, 0, 1, 7},
                {9, 4, 3, 8, 6, 1, 7, 2, 0, 5},
                {2, 5, 8, 1, 4, 3, 6, 7, 9, 0}}).

-define(LOOKUP(X, Y), (element(Y + 1, element(X + 1, ?TABLE)))).

-spec checksum(non_neg_integer()) -> 0..9.
checksum(N) ->
    S = integer_to_list(N),
    checksum(S, 0).

checksum([C|Rest], Acc) ->
    checksum(Rest, ?LOOKUP(Acc, C - 48));
checksum([], Acc) ->
    Acc.

-spec encode(non_neg_integer()) -> non_neg_integer().
encode(N) ->
    10 * N + checksum(N).

-spec is_valid(non_neg_integer()) -> boolean().
is_valid(N) ->
    checksum(N) == 0.
