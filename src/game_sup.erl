%%%-------------------------------------------------------------------
%% @doc game top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    GateServer = {
        gate_server_tag,
        {gate_server, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [gate_server]
    },
    ClientSup = {
        client_sup_tag,
        {client_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        []
    },
    NetCommand = {
        net_command_tag,
        {net_command, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [net_command]
    },
    LoginServer = {
        login_server_tag,
        {login_server, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [login_server]
    },
    PlayerSup = {
        player_sup_tag,
        {player_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        []
    },
    ChildSpecs = [
        GateServer,
        ClientSup,
        NetCommand,
        LoginServer,
        PlayerSup
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
