-module(day1).
-behavior(aoc).

-export([input_type/0, p1/1, p2/1]).

input_type() -> lines.


p1(Lines) ->
    Elves = find_top_N(1, Lines),
    lists:max(Elves).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

example(p1) ->
    input:lines("day1_a").

p1_test() ->
    ?assertEqual(24000, p1(example(p1))).

-endif.


p2(Lines) ->
    lists:sum(find_top_N(3, Lines)).


find_top_N(N, Lines) ->
    find_top_N(Lines, 0, [0 || _ <- lists:seq(1, N)]).


find_top_N([], Last, Top) ->
    tl(lists:sort([Last | Top]));

find_top_N([<<>> | Lines], Last, Top) ->
    find_top_N(Lines, 0, tl(lists:sort([Last | Top])));

find_top_N([Line | Lines], Last, Top) ->
    Calories = binary_to_integer(Line),
    find_top_N(Lines, Last + Calories, Top).


-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(45000, p2(example(p1))).

-endif.