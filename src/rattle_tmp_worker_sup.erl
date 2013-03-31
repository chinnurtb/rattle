%%% -------------------------------------------------------------------
%%% @author Łukasz Lalik <lukasz.lalik@gmail.com>
%%%
%%% @doc 
%%% @end
%%% -------------------------------------------------------------------
-module(rattle_tmp_worker_sup).
-author('lukasz.lalik@gmail.com').

-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

%% ===================================================================
%% Interface
%% ===================================================================

start_link(SupName, Name, MFA) ->
	supervisor:start_link({local, SupName}, ?MODULE, {Name, MFA}).

%% ===================================================================
%% Calbacks
%% ===================================================================

init({Name, {M, F, A}}) ->
    Child = {Name, {M, F, A},
			 temporary, brutal_kill, worker, [M]},
	
	{ok, {{simple_one_for_one, 5, 60}, [Child]}}.