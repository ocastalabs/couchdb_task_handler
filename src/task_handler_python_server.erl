-module(task_handler_python_server).
-include_lib("couch/include/couch_db.hrl").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, create_task/3]).

%% ===================================================================
%% Public API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:call(?MODULE, stop).

create_task(_, undefined, _) ->
    undefined_id;
create_task(_, _, undefined) ->
    undefined_rev;
create_task(DBName, Id, Rev) ->
    gen_server:cast(?MODULE, {create_task, DBName, Id, Rev}),
    ok.

%% ===================================================================
%% gen_server
%% ===================================================================

-record(state, {port}).

init([]) ->
    PrivDir = code:priv_dir(task_handler),
    Port = open_port({spawn, "python -u task_creator.py"},
                     [{packet, 1}, {cd, PrivDir}, binary, use_stdio]),
    {ok, #state{port = Port}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({create_task, DBName, Id, Rev}, State = #state{port = Port}) ->
    ?LOG_INFO("Creating task for ~s/~s rev: ~s",[DBName, Id, Rev]),
    ReqData = term_to_binary({create, DBName, Id, Rev}),
    port_command(Port, ReqData),
    receive
        {Port, {data, _RespData}} ->
            ok
    after
        5000 ->
            ?LOG_ERROR("No task created for ~p/~p rev: ~p", [DBName, Id, Rev])
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



