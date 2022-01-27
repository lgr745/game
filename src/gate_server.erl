%%%-------------------------------------------------------------------
%%% @author Lron
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 11月 2021 17:00
%%%-------------------------------------------------------------------
-module(gate_server).
-author("Lron").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(gate_server_state, {listener, acceptor}).

-include("gate_def.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #gate_server_state{}} | {ok, State :: #gate_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    process_flag(trap_exit, true),
    Opts = [
        binary,
        {packet, 2},
        {reuseaddr, true},
        {keepalive, true},
        {backlog, 30},
        {active, false}
    ],
    case gen_tcp:listen(?CLIENT_PORT, Opts) of
        {ok, Listen} ->
            case prim_inet:async_accept(Listen, -1) of
                {ok, Ref} ->
                    {ok, #gate_server_state{listener=Listen, acceptor=Ref}};
                {Error} ->
                    {stop, Error}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #gate_server_state{}) ->
    {reply, Reply :: term(), NewState :: #gate_server_state{}} |
    {reply, Reply :: term(), NewState :: #gate_server_state{}, timeout() | hibernate} |
    {noreply, NewState :: #gate_server_state{}} |
    {noreply, NewState :: #gate_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #gate_server_state{}} |
    {stop, Reason :: term(), NewState :: #gate_server_state{}}).
handle_call(_Request, _From, State = #gate_server_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #gate_server_state{}) ->
    {noreply, NewState :: #gate_server_state{}} |
    {noreply, NewState :: #gate_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gate_server_state{}}).
handle_cast(_Request, State = #gate_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #gate_server_state{}) ->
    {noreply, NewState :: #gate_server_state{}} |
    {noreply, NewState :: #gate_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gate_server_state{}}).
handle_info({inet_async, Lister, Ref, {ok, ClientSocket}}, #gate_server_state{listener=Lister, acceptor=Ref} = State) ->
    try
        case set_sockopt(Lister, ClientSocket) of
            ok -> ok;
            {error, Reason} -> exit({set_sockopt, Reason})
        end,

        %% New client connected - spawn a new process using the simple_one_for_one
        %% supervisor.
        {ok, _Pid} = client_sup:new_client_socket(ClientSocket),

        %% Signal the network driver that we are ready to accept another connection
        case prim_inet:async_accept(Lister, -1) of
            {ok, NewRef} -> {noreply, State#gate_server_state{acceptor=NewRef}};
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end
    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
        {stop, Why, State}
    end;
handle_info({inet_async, Lister, Ref, Error}, #gate_server_state{listener=Lister, acceptor=Ref} = State) ->
    {stop, Error, State};
handle_info(_Info, State = #gate_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #gate_server_state{}) -> term()).
terminate(_Reason, _State = #gate_server_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #gate_server_state{},
    Extra :: term()) ->
    {ok, NewState :: #gate_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #gate_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

set_sockopt(Lister, ClientSocket) ->
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case prim_inet:getopts(Lister, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(ClientSocket, Opts) of
                ok    -> ok;
                Error -> gen_tcp:close(ClientSocket), Error
            end;
        Error ->
            gen_tcp:close(ClientSocket), Error
    end.
