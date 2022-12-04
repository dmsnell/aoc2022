-module(day4).

-export([input_type/0, parse_input/1, p1/1, p2/1]).


input_type() -> lines.

parse_input(Lines) ->
    [
        [[binary_to_integer(Section) || Section <- binary:split(Elf, <<"-">>)] || Elf <- binary:split(Line, <<",">>)]
        ||
        Line <- Lines
    ].


p1(Pairs) ->
    p1(Pairs, 0).

p1([], Overlapping) ->
    Overlapping;

p1([[[La, Lb], [Ra, Rb]] | Pairs], Overlapping) ->
    if
        La =< Ra, Lb >= Rb -> p1(Pairs, Overlapping + 1);
        Ra =< La, Rb >= Lb -> p1(Pairs, Overlapping + 1);
        true               -> p1(Pairs, Overlapping)
    end.


p2(Pairs) ->
    p2(Pairs, 0).

p2([], Overlapping) ->
    Overlapping;

p2([Pair | Pairs], Overlapping) ->
    [[La, Lb], [Ra, _]] = lists:sort(Pair),
    if
        La =< Ra andalso Lb >= Ra -> p2(Pairs, Overlapping + 1);
        true                      -> p2(Pairs, Overlapping)
    end.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(2, p1(parse_input(input:lines("day4_a")))).

p1_answer_test() ->
    ?assertMatch({day4, p1, 444, _}, aoc:solve(day4, p1)).

p2_test() ->
    ?assertEqual(4, p2(parse_input(input:lines("day4_a")))).

p2_answer_test() ->
    ?assertMatch({day4, p2, 801, _}, aoc:solve(day4, p2)).

-endif.
