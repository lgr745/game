%%%-------------------------------------------------------------------
%%% @author Lron
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 12月 2021 19:52
%%%-------------------------------------------------------------------
-module(net_command).
-author("Lron").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(net_command_state, {proto_dict = orddict:new()}).

%%%===================================================================
%%% API
%%%===================================================================

-export([register_net_command/2, handle_net_command/4]).
register_net_command(ProtoFile, Module) ->
    gen_server:call(?MODULE, {register, ProtoFile, Module}).

handle_net_command(ProtoFile, SocketPid, Msg, PackBin) ->
    gen_server:cast(?MODULE, {handle, ProtoFile, SocketPid, Msg, PackBin}).

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
    {ok, State :: #net_command_state{}} | {ok, State :: #net_command_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #net_command_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #net_command_state{}) ->
    {reply, Reply :: term(), NewState :: #net_command_state{}} |
    {reply, Reply :: term(), NewState :: #net_command_state{}, timeout() | hibernate} |
    {noreply, NewState :: #net_command_state{}} |
    {noreply, NewState :: #net_command_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #net_command_state{}} |
    {stop, Reason :: term(), NewState :: #net_command_state{}}).
handle_call({register, ProtoFile, Module}, _From, State = #net_command_state{proto_dict = ProtoDict}) ->
    NewProtoDict = orddict:store(ProtoFile, Module, ProtoDict),
    {reply, ok, State#net_command_state{proto_dict = NewProtoDict}};
handle_call(_Request, _From, State = #net_command_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #net_command_state{}) ->
    {noreply, NewState :: #net_command_state{}} |
    {noreply, NewState :: #net_command_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #net_command_state{}}).
handle_cast({handle, ProtoFile, SocketPid, Msg, PackBin}, State = #net_command_state{proto_dict = ProtoDict}) ->
    case orddict:find(ProtoFile, ProtoDict) of
        {ok, Module} -> Module:on_command(SocketPid, Msg, PackBin);
        error -> {error, proto_file_handle_fail}
    end,
    {noreply, State};
handle_cast(_Request, State = #net_command_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #net_command_state{}) ->
    {noreply, NewState :: #net_command_state{}} |
    {noreply, NewState :: #net_command_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #net_command_state{}}).
handle_info(_Info, State = #net_command_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #net_command_state{}) -> term()).
terminate(_Reason, _State = #net_command_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #net_command_state{},
    Extra :: term()) ->
    {ok, NewState :: #net_command_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #net_command_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
