-module(day6).

-export([input_type/0, p1/1, p2/1]).


input_type() -> raw.


p1(Buffer) ->
    find_signal(Buffer).


p2(Buffer) ->
    find_message(Buffer).


find_signal(Buffer) ->
    find_unique(Buffer, 4).

find_message(Buffer) ->
    find_unique(Buffer, 14).


find_unique(Buffer, N) ->
    Prefix = binary_to_list(binary:part(Buffer, {0, N})),
    Rest   = binary:part(Buffer, {N, size(Buffer) - N}),
    find_unique(Rest, Prefix, length(Prefix)).

find_unique(<<L, Buffer/binary>>, [_Oldest | LastThree] = Seen, Index) ->
    Size = length(Seen),
    case length(lists:uniq(Seen)) of
        Size -> Index;
        _    -> find_unique(Buffer, LastThree ++ [L], Index + 1)
    end.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(7, p1(<<"mjqjpqmgbljsphdztnvjfqwrcgsmlb">>)),
    ?assertEqual(5, p1(<<"bvwbjplbgvbhsrlpgdmjqwftvncz">>)),
    ?assertEqual(6, p1(<<"nppdvjthqldpwncqszvftbrmjlhg">>)),
    ?assertEqual(10, p1(<<"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg">>)),
    ?assertEqual(11, p1(<<"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw">>)).


p1_answer_test() ->
    ?assertMatch({day6, p1, 1544, _}, aoc:solve(day6, p1)).

p2_test() ->
    ?assertEqual(19, p2(<<"mjqjpqmgbljsphdztnvjfqwrcgsmlb">>)),
    ?assertEqual(23, p2(<<"bvwbjplbgvbhsrlpgdmjqwftvncz">>)),
    ?assertEqual(23, p2(<<"nppdvjthqldpwncqszvftbrmjlhg">>)),
    ?assertEqual(29, p2(<<"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg">>)),
    ?assertEqual(26, p2(<<"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw">>)).

p2_answer_test() ->
    ?assertMatch({day6, p2, 2145, _}, aoc:solve(day6, p2)).

-endif.