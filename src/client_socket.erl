%%%-------------------------------------------------------------------
%%% @author Lron
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 11月 2021 23:25
%%%-------------------------------------------------------------------
-module(client_socket).
-author("Lron").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
    code_change/4, callback_mode/0]).

-export([set_socket_ready/2, send_pack/3]).

-define(SERVER, ?MODULE).

-record(client_socket_state, {socket}).

-include("client_def.hrl").
-include("base_pb.hrl").

%%%===================================================================
%%% API
%%%===================================================================

set_socket_ready(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_statem:cast(Pid, {socket_ready, Socket}).

send_pack(Pid, ProtoModule, PackRecord) when is_pid(Pid) ->
    ServerPack = base_pb:encode_msg(#'S2CPack'{
        file = ProtoModule:source_basename(),
        msg = atom_to_list(element(1, PackRecord)),
        pack = ProtoModule:encode_msg(PackRecord)
    }),
    gen_statem:cast(Pid, {socket_send, ServerPack}).

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
    process_flag(trap_exit, true),
    {ok, wait_for_socket, #client_socket_state{}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    handle_event_function.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #client_socket_state{}) ->
    NextStateName = next_state,
    {next_state, NextStateName, State}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(cast, {socket_ready, Socket}, wait_for_socket, _State) ->
    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    {next_state, socket_success, #client_socket_state{socket = Socket}, {timeout, ?SOCKET_TIMEOUT, {conn_timeout}}};

handle_event(cast, {socket_send, ServerPack}, socket_success, #client_socket_state{socket = Socket} = State) ->
    gen_tcp:send(Socket, ServerPack),
    {next_state, socket_success, State, {timeout, ?SOCKET_TIMEOUT, {conn_timeout}}};

handle_event(timeout, {conn_timeout}, socket_success, State) ->
    io:format("Client Socket timeout~n"),
    {stop, normal, State};

handle_event(info, {tcp, Socket, Bin}, socket_success, #client_socket_state{socket = Socket} = State) ->
    on_command(Socket, Bin),
    inet:setopts(Socket, [{active, once}]),
    {next_state, socket_success, State, {timeout, ?SOCKET_TIMEOUT, {conn_timeout}}};

handle_event(info, {tcp_error, _Socket, _Reason}, _StateName, State) ->
    io:format("Client Socket error~n"),
    {stop, normal, State};

handle_event(info, {tcp_closed, _Socket}, _StateName, State) ->
    io:format("Client Socket closed~n"),
    {stop, normal, State};

handle_event(_EventType, _EventContent, _StateName, State = #client_socket_state{}) ->
    NextStateName = the_next_state_name,
    {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #client_socket_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #client_socket_state{}, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

on_command(_Socket, Bin) ->
    ClientPack = base_pb:decode_msg(Bin, 'C2SPack'),
    io:format("Server Bin = ~p~n", [ClientPack]),
    #'C2SPack'{file=ProtoFile, msg=MsgName, pack=PackBin} = ClientPack,
    Msg = list_to_atom(MsgName),
    net_command:handle_net_command(ProtoFile, self(), Msg, PackBin).
