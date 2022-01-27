%%%-------------------------------------------------------------------
%% @doc game public API
%% @end
%%%-------------------------------------------------------------------

-module(game_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("game_pb.hrl").

start(_StartType, _StartArgs) ->
    game_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
