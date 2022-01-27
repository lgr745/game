%%%-------------------------------------------------------------------
%%% @author Lron
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 11月 2021 23:44
%%%-------------------------------------------------------------------
-module(client_sup).
-author("Lron").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-export([new_client_socket/1]).
new_client_socket(ClientSocket) ->
    {ok, Pid} = supervisor:start_child(?MODULE, []),
    gen_tcp:controlling_process(ClientSocket, Pid),
    client_socket:set_socket_ready(Pid, ClientSocket),
    {ok, Pid}.

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]}}
    | ignore | {error, Reason :: term()}).
init([]) ->
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = #{strategy => simple_one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},

    AChild = #{id => undefined,
        start => {client_socket, start_link, []},
        restart => temporary,
        shutdown => 2000,
        type => worker,
        modules => []},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
