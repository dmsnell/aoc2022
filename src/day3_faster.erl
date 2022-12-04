-module(day3_faster).
-behavior(aoc).

-export([input_type/0, p1/1, p2/1]).


input_type() -> lines.


p1(Lines) ->
    p1(Lines, 0).

p1([], Priority) ->
    Priority;

p1([Line | Lines], Priority) ->
    [Left, Right] = mid_split(Line),
    Types  = #{},
    Types1 = set_types(Left, Types, 0, 1),
    NewPriority = check_types(Right, Types1, 1),
    p1(Lines, Priority + NewPriority).


p2(Lines) ->
    p2(Lines, 0).

p2([<<>>], Priority) ->
    p2([], Priority);

p2([], Priority) ->
    Priority;

p2([A, B, C | Lines], Priority) ->
    Types  = #{},
    Types1 = set_types(A, Types, 0, 1),
    Types2 = set_types(B, Types1, 1, 2),
    NewPriority = check_types(C, Types2, 2),
    p2(Lines, Priority + NewPriority).


set_types(<<>>, Types, _E, _V) ->
    Types;
set_types(<<C, Cs/binary>>, Types, E, V) ->
    TC = type(C),
    Types1 = case maps:get(TC, Types, 0) of
        E -> maps:put(TC, V, Types);
        _ -> Types
    end,
    set_types(Cs, Types1, E, V).


check_types(Line, Types, Threshold) ->
    check_types(Line, Types, Threshold, #{}).

check_types(<<>>, _Types, _Threshold, Common) ->
    lists:sum(maps:keys(Common));
check_types(<<C, Cs/binary>>, Types, Threshold, Common) ->
    TC = type(C),
    Common1 = case maps:get(TC, Types, 0) of
        Threshold -> maps:put(TC, 1, Common);
        _         -> Common
    end,
    check_types(Cs, Types, Threshold, Common1).


type(C) ->
    if
        C >= $a -> C - $a + 1;
        true    -> C - $A + 27
    end.


mid_split(Line) ->
    Mid        = size(Line) div 2,
    Left       = binary:part(Line, {0, Mid}),
    Right      = binary:part(Line, {Mid, size(Line) - Mid}),
    [Left, Right].


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(157, p1(input:lines("day3_a"))).

p1_answer_test() ->
    ?assertMatch({day3_faster, p1, 8515, _}, aoc:solve(day3_faster, p1)).

p2_test() ->
    ?assertEqual(70, p2(input:lines("day3_b"))).

p2_answer_test() ->
    ?assertMatch({day3_faster, p2, 2434, _}, aoc:solve(day3_faster, p2)).

-endif.