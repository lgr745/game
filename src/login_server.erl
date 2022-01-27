%%%-------------------------------------------------------------------
%%% @author Lron
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 12月 2021 22:43
%%%-------------------------------------------------------------------
-module(login_server).
-author("Lron").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(login_server_state, {}).

-include("login_pb.hrl").

%%%===================================================================
%%% API
%%%===================================================================


-export([on_command/3]).
on_command(SocketPid, Msg, Bin) ->
    ClientPack = login_pb:decode_msg(Bin, Msg),
    gen_server:cast(?MODULE, {c2s, SocketPid, ClientPack}).

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
    {ok, State :: #login_server_state{}} | {ok, State :: #login_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    net_command:register_net_command(login_pb:source_basename(), ?MODULE),
    {ok, #login_server_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #login_server_state{}) ->
    {reply, Reply :: term(), NewState :: #login_server_state{}} |
    {reply, Reply :: term(), NewState :: #login_server_state{}, timeout() | hibernate} |
    {noreply, NewState :: #login_server_state{}} |
    {noreply, NewState :: #login_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #login_server_state{}} |
    {stop, Reason :: term(), NewState :: #login_server_state{}}).
handle_call(_Request, _From, State = #login_server_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #login_server_state{}) ->
    {noreply, NewState :: #login_server_state{}} |
    {noreply, NewState :: #login_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #login_server_state{}}).
handle_cast({c2s, SocketPid, _Info = #'LoginRequest'{account = Account, time = LoginTime}}, State = #login_server_state{}) ->
    io:format("Account ~p Time ~p~n", [Account, LoginTime]),
    LoginResponse = #'LoginResponse'{result = 1, reason = ""},
    client_socket:send_pack(SocketPid, login_pb, LoginResponse),
    {noreply, State};
handle_cast(_Request, State = #login_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #login_server_state{}) ->
    {noreply, NewState :: #login_server_state{}} |
    {noreply, NewState :: #login_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #login_server_state{}}).
handle_info(_Info, State = #login_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #login_server_state{}) -> term()).
terminate(_Reason, _State = #login_server_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #login_server_state{},
    Extra :: term()) ->
    {ok, NewState :: #login_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #login_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
