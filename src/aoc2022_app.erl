%%%-------------------------------------------------------------------
%% @doc aoc2022 public API
%% @end
%%%-------------------------------------------------------------------

-module(aoc2022_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    aoc2022_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
