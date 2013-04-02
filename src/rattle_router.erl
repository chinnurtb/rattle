%%% -------------------------------------------------------------------
%%% @author Łukasz Lalik <lukasz.lalik@gmail.com>
%%%
%%% @doc 
%%% @end
%%% -------------------------------------------------------------------
-module(rattle_router).
-author('lukasz.lalik@gmail.com').

-behavior(gen_server).

-export([start_link/1, login/0, heartbeat/2, push/2, session_list/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 code_change/3, terminate/2]).

-include("internal_messages.hrl").

-record(state, {sids, pids, client_sup, broker_sup}).

-define(CLIENT_WORKER(MFA), 
		{client_worker_sup, 
		 {rattle_tmp_worker_sup, start_link, 
		  [rattle_client_worker_sup,
		   rattle_client, MFA]
		 },
		 permanent,
		 infinity,
		 worker,
		 [rattle_tmp_worker_sup]
		}
	   ).

-define(BROKER_WORKER(MFA), 
		{broker_worker_sup, 
		 {rattle_tmp_worker_sup, start_link, 
		  [rattle_client_broker_worker_sup,
		   rattle_client_broker, MFA]
		 },
		 permanent,
		 infinity,
		 worker,
		 [rattle_tmp_worker_sup]
		}
	   ).

%% ===================================================================
%% Interface
%% ===================================================================

start_link(Sup) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Sup, []).

login() ->
	gen_server:call(?MODULE, login).

heartbeat(Sid, ReplyChannel) ->
	gen_server:cast(?MODULE, {heartbeat, Sid, ReplyChannel}).

push(Sid, Message) ->
	gen_server:cast(?MODULE, {push, Sid, Message}).

% @TODO Not completly sure about this function...
session_list() ->
	gen_server:call(?MODULE, session_list).

%% ===================================================================
%% Calbacks
%% ===================================================================

init(Sup) ->
	gen_server:cast(self(), {after_init, Sup}),
	{ok, #state{
				sids = ets:new(?MODULE, [set]),
				pids = ets:new(?MODULE, [set])
			   }}.

handle_call(login, _From, State) ->
	Sid = rattle_utils:generate_sid(),

	{ok, ClientBrokerPid} = supervisor:start_child(State#state.broker_sup, 
												   [Sid, State#state.client_sup]),
	monitor(process, ClientBrokerPid),

	ets:insert(State#state.sids, {Sid, ClientBrokerPid}),
	ets:insert(State#state.pids, {ClientBrokerPid, Sid}),

	lager:info("New client connected with SID: ~s", [Sid]),
	{reply, Sid, State};

handle_call(session_list, _From, State) ->
	Sids = build_session_list(State#state.sids, ets:first(State#state.sids), []),
	{reply, Sids, State}.


handle_cast({after_init, Sup}, State) ->
	{ok, BrokerSupPid}    = supervisor:start_child(Sup,
							?BROKER_WORKER({rattle_client_broker, start_link, []})),

	{ok, ClientSupPid} = supervisor:start_child(Sup,
							?CLIENT_WORKER({rattle_client, start_link, []})),

	NewState = State#state{client_sup = ClientSupPid,
						   broker_sup = BrokerSupPid},
	{noreply, NewState};

handle_cast({push, Sid, Message}, State) ->
	case ets:lookup(State#state.sids, Sid) of 
		[{Sid, BrokerPid}] ->
			rattle_client_broker:push(BrokerPid, Message);

		_ -> 
			% @TODO Error handling
			ok
	end,
	{noreply, State};

handle_cast({heartbeat, Sid, ReplyChannel}, State) ->
	case ets:lookup(State#state.sids, Sid) of 
		[{Sid, BrokerPid}] ->
			rattle_client_broker:heartbeat(BrokerPid, ReplyChannel);

		_ ->
			ReplyChannel(#imsg{level = server,
							   type  = internal_error,
							   payload = <<"Unrecognized SID">>})
	end,
	{noreply, State}.


handle_info({'DOWN', MonitorRef, process, BrokerPid, _}, State) ->
	case ets:lookup(State#state.pids, BrokerPid) of
		[] -> 
			ok;
		[{BrokerPid, Sid}] ->
			lager:info("Client disconnected SID: ~s", [Sid]),
			ets:delete(State#state.pids, BrokerPid),
			ets:delete(State#state.sids, Sid)
	end,
	{noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

build_session_list(_, '$end_of_table', List) ->
	List;
build_session_list(Sids, Sid, List) ->
	BSid = list_to_binary(Sid),
	build_session_list(Sids, ets:next(Sids, Sid), [BSid | List]).
