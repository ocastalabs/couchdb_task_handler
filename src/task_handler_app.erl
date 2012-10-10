-module(task_handler_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    task_handler_sup:start_link().

stop(_State) ->
    ok.
