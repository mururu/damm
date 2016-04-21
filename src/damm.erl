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

-spec checksum(binary() | list() | non_neg_integer()) -> 0..9.
checksum(Bin) when is_binary(Bin) ->
    checksum_bin(Bin, 0);
checksum(S) when is_list(S) ->
    checksum_list(S, 0);
checksum(N) when is_integer(N) ->
    S = integer_to_list(N),
    checksum_list(S, 0).

checksum_bin(<<C, Rest/binary>>, Acc) ->
    checksum_bin(Rest, ?LOOKUP(Acc, C - 48));
checksum_bin(<<>>, Acc) ->
    Acc.

checksum_list([C|Rest], Acc) ->
    checksum_list(Rest, ?LOOKUP(Acc, C - 48));
checksum_list([], Acc) ->
    Acc.

-spec encode(binary() | list() | non_neg_integer()) -> binary() | list() | non_neg_integer().
encode(Bin) when is_binary(Bin) ->
    <<Bin/binary, (checksum_bin(Bin, 0) + 48)>>;
encode(S) when is_list(S) ->
    S ++ [checksum_list(S, 0) + 48];
encode(N) when is_integer(N) ->
    S = integer_to_list(N),
    10 * N + checksum_list(S, 0).

-spec is_valid(binary() | list() | non_neg_integer()) -> boolean().
is_valid(N) ->
    checksum(N) == 0.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

checksum_test_() ->
    [?_assertEqual(4, checksum(572)),
     ?_assertEqual(4, checksum("572")),
     ?_assertEqual(4, checksum(<<"572">>))].

encode_test_() ->
    [?_assertEqual(5724, encode(572)),
     ?_assertEqual("5724", encode("572")),
     ?_assertEqual(<<"5724">>, encode(<<"572">>))].

is_valid_test_() ->
    [?_assertEqual(true, is_valid(5724)),
     ?_assertEqual(true, is_valid("5724")),
     ?_assertEqual(true, is_valid(<<"5724">>))].

-endif.
