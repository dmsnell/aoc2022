-module(day2).

-export([input_type/0, parse_input/1, p1/1, p2/1]).

input_type() -> lines.

parse_input(Lines) ->
    Pairs = [string:lexemes(Line, " ") || Line <- Lines],
    [{decrypt(Left), decrypt(Right)} || [Left, Right] <- Pairs].

p1(Moves) ->
    Rounds = [score(Mine, Theirs) || {Theirs, {Mine, _}} <- Moves],
    lists:sum(Rounds).


decrypt(<<"A">>) -> rock;
decrypt(<<"B">>) -> paper;
decrypt(<<"C">>) -> scissors;
decrypt(<<"X">>) -> {rock, lose};
decrypt(<<"Y">>) -> {paper, draw};
decrypt(<<"Z">>) -> {scissors, win}.


score(rock)     -> 1;
score(paper)    -> 2;
score(scissors) -> 3.

score(Mine, Theirs) ->
    Game = case beats(Mine) of
        Theirs                -> 0;
        _ when Mine == Theirs -> 3;
        _                     -> 6
    end,
    score(Mine) + Game.


beats(rock)     -> paper;
beats(paper)    -> scissors;
beats(scissors) -> rock.


-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

p1_test() ->
    ?assertEqual(15, p1(parse_input(input:lines("day2_a")))).

p1_answer_test() ->
    ?assertMatch({day2, p1, 13675, _}, aoc:solve(day2, p1)).

-endif.

p2(Moves) ->
    Rounds = [score(for_outcome(Outcome, Theirs), Theirs) || {Theirs, {_, Outcome}} <- Moves],
    lists:sum(Rounds).


for_outcome( win, Theirs) -> beats(Theirs);
for_outcome(lose, Theirs) -> beats(beats(Theirs));
for_outcome(draw, Theirs) -> Theirs.


-ifdef(EUNIT).

p2_test() ->
    ?assertEqual(12, p2(parse_input(input:lines("day2_a")))).

p2_answer_test() ->
    ?assertMatch({day2, p2, 14184, _}, aoc:solve(day2, p2)).

-endif.