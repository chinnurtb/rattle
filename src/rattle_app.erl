-module(rattle_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> 
	application:start(rattle).

start(_StartType, _StartArgs) ->
    rattle_sup:start_link().

stop(_State) ->
    ok.
