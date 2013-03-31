%%% -------------------------------------------------------------------
%%% @author Łukasz Lalik <lukasz.lalik@gmail.com>
%%%
%%% @doc 
%%% @end
%%% -------------------------------------------------------------------
-module(rattle_pool_sup).
-author('lukasz.lalik@gmail.com').

-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% Interface
%% ===================================================================

start_link(SupName, PoolManager) ->
	supervisor:start_link({local, SupName}, ?MODULE, PoolManager).

%% ===================================================================
%% Calbacks
%% ===================================================================

init(PoolManagerServer) ->
	PoolManager = 
		{pool_manager, 
		 {PoolManagerServer, start_link, [self()]},
		 permanent,
		 brutal_kill,
		 worker,
		 [PoolManagerServer]
		},

	{ok, { {one_for_all, 0, 3600}, [PoolManager] }}.