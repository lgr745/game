%%%-------------------------------------------------------------------
%%% @author Lron
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 12月 2021 23:47
%%%-------------------------------------------------------------------
-module(ply_base).
-author("Lron").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(ply_base_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Args :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(PlayerID) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PlayerID], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #ply_base_state{}} | {ok, State :: #ply_base_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([PlayerID]) ->                 
    io:format("PlayerID ~p~n", [PlayerID]),
    {ok, #ply_base_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #ply_base_state{}) ->
    {reply, Reply :: term(), NewState :: #ply_base_state{}} |
    {reply, Reply :: term(), NewState :: #ply_base_state{}, timeout() | hibernate} |
    {noreply, NewState :: #ply_base_state{}} |
    {noreply, NewState :: #ply_base_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #ply_base_state{}} |
    {stop, Reason :: term(), NewState :: #ply_base_state{}}).
handle_call(_Request, _From, State = #ply_base_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #ply_base_state{}) ->
    {noreply, NewState :: #ply_base_state{}} |
    {noreply, NewState :: #ply_base_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #ply_base_state{}}).
handle_cast(_Request, State = #ply_base_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #ply_base_state{}) ->
    {noreply, NewState :: #ply_base_state{}} |
    {noreply, NewState :: #ply_base_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #ply_base_state{}}).
handle_info(_Info, State = #ply_base_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #ply_base_state{}) -> term()).
terminate(_Reason, _State = #ply_base_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #ply_base_state{},
    Extra :: term()) ->
    {ok, NewState :: #ply_base_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #ply_base_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
