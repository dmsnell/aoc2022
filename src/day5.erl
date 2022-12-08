-module(day5).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.


parse_input(Lines) ->
    {Stacks, MoveLines} = parse_stacks(Lines),
    Moves = parse_moves(MoveLines),
    {Stacks, Moves}.


parse_stacks(Lines) ->
    parse_stacks(Lines, #{}).

parse_stacks([<<" 1 ", _/binary>>, <<>> | MoveLines], Stacks) ->
    Stacks1 = maps:map(fun (_K, V) -> lists:reverse(V) end, Stacks),
    {Stacks1, MoveLines};

parse_stacks([Line | Lines], Stacks) ->
    parse_stacks(Lines, parse_stack_line(Line, Stacks)).


parse_stack_line(Line, Stacks) ->
    parse_stack_line(<<Line/binary, " ">>, Stacks, 1).

parse_stack_line(<<>>, Stacks, _Stack) ->
    Stacks;

parse_stack_line(<<_, C, _, " ", Line/binary>>, Stacks, Stack) ->
    NextStacks = case C of
        $  -> Stacks;
        _  -> maps:update_with(Stack, fun (S) -> [C | S] end, [C], Stacks)
    end,
    parse_stack_line(Line, NextStacks, Stack + 1).


parse_moves(Lines) ->
    parse_moves(Lines, []).

parse_moves([], Moves) ->
    lists:reverse(Moves);
parse_moves([Line | Lines], Moves) ->
    [<<"move">>, Count, <<"from">>, From, <<"to">>, To] = binary:split(Line, <<" ">>, [global]),
    Move = lists:map(fun binary_to_integer/1, [Count, From, To]),
    parse_moves(Lines, [Move | Moves]).


p1({Stacks, Moves}) ->
    FinalStacks = apply_moves(Moves, Stacks, crate_mover_9000),
    [hd(maps:get(Stack, FinalStacks)) || Stack <- lists:sort(maps:keys(Stacks))].


apply_moves([], Stacks, _Crane) ->
    Stacks;

apply_moves([[Count, From, To] | Moves], Stacks, Crane) ->
    {Moved, FromStack} = lists:split(Count, maps:get(From, Stacks)),
    Stacks1 = maps:put(From, FromStack, Stacks),
    MovedTo = case Crane of
        crate_mover_9000 -> lists:reverse(Moved);
        crate_mover_9001 -> Moved
    end,
    Stacks2 = maps:update_with(To, fun (Stack) -> MovedTo ++ Stack end, Stacks1),
    apply_moves(Moves, Stacks2, Crane).


p2({Stacks, Moves}) ->
    FinalStacks = apply_moves(Moves, Stacks, crate_mover_9001),
    [hd(maps:get(Stack, FinalStacks)) || Stack <- lists:sort(maps:keys(Stacks))].


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual("CMZ", p1(parse_input(input:lines("day5_a")))).

p1_answer_test() ->
    ?assertMatch({day5, p1, "PSNRGBTFT", _}, aoc:solve(day5, p1)).


p2_test() ->
    ?assertEqual("MCD", p2(parse_input(input:lines("day5_a")))).


p2_answer_test() ->
    ?assertMatch({day5, p2, "BNTZFPMMW", _}, aoc:solve(day5, p2)).

-endif.
