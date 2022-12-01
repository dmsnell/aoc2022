-module(day1).
-behavior(aoc).

-export([input_type/0, p1/1, p2/1]).

input_type() -> lines.


p1(Lines) ->
    Elves = [lists:sum(Elf) || Elf <- input_groups(Lines)],
    lists:max(Elves).


input_groups(Lines) ->
    input_groups(Lines, [[]]).

input_groups([], [[] | Elves]) ->
    Elves;

input_groups([], Elves) ->
    Elves;

input_groups([<<>> | Lines], Elves) ->
    input_groups(Lines, [[] | Elves]);

input_groups([Line | Lines], [Elf | Elves]) ->
    Calories = binary_to_integer(Line),
    input_groups(Lines, [[Calories | Elf] | Elves]).


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

example(p1) ->
    input:lines("day1_a").

p1_test() ->
    ?assertEqual(24000, p1(example(p1))).

-endif.


p2(Lines) ->
    lists:sum(find_top_three(Lines)).


find_top_three(Lines) ->
    find_top_three(Lines, 0, [0, 0, 0]).


find_top_three([], Last, TopThree) ->
    tl(lists:sort([Last | TopThree]));

find_top_three([<<>> | Lines], Last, TopThree) ->
    find_top_three(Lines, 0, tl(lists:sort([Last | TopThree])));

find_top_three([Line | Lines], Last, TopThree) ->
    Calories = binary_to_integer(Line),
    find_top_three(Lines, Last + Calories, TopThree).


-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(45000, p2(example(p1))).

-endif.