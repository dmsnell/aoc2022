-module(day8).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.

parse_input(Lines) ->
    {Rows, Cols, FinalGrid} = lists:foldl(
        fun (Line, {Row, _Cols, PrevGrid}) ->
            {Cols, NextGrid} = lists:foldl(
                fun (N, {Col, Grid}) -> {Col + 1, Grid#{{Col, Row} => list_to_integer([N])}} end,
                {1, PrevGrid},
                erlang:binary_to_list(Line)
            ),
            {Row + 1, Cols, NextGrid}
        end,
        {1, 1, #{}},
        Lines
    ),
    {{Rows - 1, Cols - 1}, FinalGrid}.

p1(Grid) ->
    visible(Grid).


p2(_Lines) ->
    ok.


visible(Grid) ->
    Visible = lists:foldl(
        fun (Dir, Visible) -> maps:merge(Visible, visible_from(Dir, Grid)) end,
        #{},
        [north, east, south, west]
    ),
    maps:size(Visible).


visible_from(Direction, {{Rows, Cols}, Grid}) ->
    Coords = fun
        (XY, Max, up)   -> [{XY, V} || V <- lists:seq(1, Max)];
        (XY, Max, down) -> [{XY, V} || V <- lists:seq(Max, 1, -1)]
    end,
    {Outies, Innies} = case Direction of
        north -> {Coords(x, Cols, up), Coords(y, Rows, up)};
        south -> {Coords(x, Cols, up), Coords(y, Rows, down)};
        east  -> {Coords(y, Rows, up), Coords(x, Cols, down)};
        west  -> {Coords(y, Rows, up), Coords(x, Cols, up)}
    end,
    lists:foldl(
        fun (O, Outer) ->
            { _H, Inner } = lists:foldl(
                fun
                    (I, {PrevH, Inner}) ->
                        Coord = case {O, I} of
                            {{x, X}, {y, Y}} -> {X, Y};
                            {{y, Y}, {x, X}} -> {X, Y}
                        end,
                        case maps:get(Coord, Grid) of
                            H when H > PrevH ->
                                {H, Inner#{Coord => H}};
                            _ ->
                                {PrevH, Inner}
                        end
                end,
                {-1, Outer},
                Innies
            ),
            Inner
        end,
        #{},
        Outies 
    ).


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertMatch({day8, p1, 21, _}, aoc:solve(day8, p1, "day8_a")).

p1_answer_test() ->
    ?assertMatch({day8, p1, 1713, _}, aoc:solve(day8, p1)).

% p2_test() ->
%     ?assertMatch({day8, p2, 1, _}, aoc:solve(day8, p2, "day8_a")).

-endif.