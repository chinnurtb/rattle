%%% -------------------------------------------------------------------
%%% @author Łukasz Lalik <lukasz.lalik@gmail.com>
%%%
%%% @doc 
%%% @end
%%% -------------------------------------------------------------------
-module(rattle_sup).
-author('lukasz.lalik@gmail.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Router = 
		{rattle_router,
		 {rattle_pool_sup, start_link, [rattle_router_sup, rattle_router]},
		 permanent,
		 infinity,
		 supervisor,
		 [rattle_pool_sup]
		 },

	ClientReqDispatcher = 
		{rattle_client_req_dispatcher,
		 {rattle_client_req_dispatcher, start_link, []},
		 permanent,
		 brutal_kill,
		 worker,
		 [rattle_client_req_dispatcher]
		},

	ApiReqDispatcher = 
		{rattle_api_req_dispatcher,
		 {rattle_api_req_dispatcher, start_link, []},
		 permanent,
		 brutal_kill,
		 worker,
		 [rattle_api_req_dispatcher]
		},
    {ok, {{one_for_one, 5, 10}, 
    	  [
    	   Router,
    	   ClientReqDispatcher,
    	   ApiReqDispatcher
    	  ]}}.

