-module(day3).
-behavior(aoc).

-export([input_type/0, p1/1, p2/1]).


input_type() -> lines.


p1(Lines) ->
    prioritize(lists:map(fun mid_split/1, Lines)).


prioritize(Groups) ->
    Commons = [maps:keys(common_items(Group)) || Group <- Groups],
    lists:sum(lists:map(fun priority/1, lists:flatten(Commons))).

priority(C) when C >= $a andalso C =< $z -> C - $a + 1;
priority(C) when C >= $A andalso C =< $Z -> C - $A + 27.

mid_split(Line) ->
    Mid        = size(Line) div 2,
    Left       = binary:part(Line, {0, Mid}),
    Right      = binary:part(Line, {Mid, size(Line) - Mid}),
    [Left, Right].

common_items([Sack | Sacks]) ->
    lists:foldl(fun (Left, Right) -> maps:intersect(items(Left), Right) end, items(Sack), Sacks).


items(Line) ->
    maps:from_list([{C, true} || C <- binary_to_list(Line)]).


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(157, p1(input:lines("day3_a"))).

p1_answer_test() ->
    ?assertMatch({day3, p1, 8515, _}, aoc:solve(day3, p1)).

-endif.


p2(Lines) ->
    prioritize(listlib:chunks(3, Lines)).


-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(70, p2(input:lines("day3_b"))).

p2_answer_test() ->
    ?assertMatch({day3, p2, 2434, _}, aoc:solve(day3, p2)).

-endif.