%%% -------------------------------------------------------------------
%%% @author Łukasz Lalik <lukasz.lalik@gmail.com>
%%%
%%% @doc 
%%% @end
%%% -------------------------------------------------------------------
-module(rattle_client_broker).
-author('lukasz.lalik@gmail.com').

-behavior(gen_fsm).

-export([start_link/2, heartbeat/2, push/2]).
-export([transient/2, processing/2, listening/2, buffering/2]).
-export([init/1, handle_event/3, handle_sync_event/4,
		 handle_info/3, terminate/3, code_change/4]).

-include("internal_messages.hrl").

-record(state, {dc_timer, flush_timer, queue, client_pid}).

-define(DC_TIMEOUT, 20 * 1000).
-define(HEARTBEAT_TIMEOUT, 15 * 1000).
-define(FLUSH_TIMEOUT, 1000).

%% ===================================================================
%% Interface
%% ===================================================================

start_link(Sid, ClientSup) ->
	gen_fsm:start_link(?MODULE, {Sid, ClientSup}, []).

heartbeat(Pid, ReplyChannel) ->
	gen_fsm:send_event(Pid, {heartbeat, ReplyChannel}).

push(Pid, Message) ->
	gen_fsm:send_event(Pid, {push, Message}).

%% ===================================================================
%% States
%% ===================================================================
transient({heartbeat, ReplyChannel}, StateData) ->
	rattle_client:connect(StateData#state.client_pid, ReplyChannel),
	{next_state, processing, refresh_dc_timer(StateData)};

transient(disconnect, StateData) ->
	{stop, normal, StateData}.

%%
%% PROCESSING state
%%
processing({heartbeat, ReplyChannel}, StateData) ->
	cancel_dc_timer(StateData),
	rattle_client:heartbeat(StateData#state.client_pid, ReplyChannel),
	case queue:is_empty(StateData#state.queue) of
		true ->
			{next_state, listening, refresh_heartbeat_timer(StateData)};

		false ->
			{next_state, buffering, refresh_flush_timer(StateData)}
	end;

processing({push, Message}, StateData) ->
	{next_state, processing, append_to_queue(StateData, Message)};

processing(disconnect, StateData) ->
	{stop, normal, StateData}.

%%
%% LISTENING state
%%
listening(timeout, StateData) ->
	Noop = #out_imsg{level = socketio,
					 type  = noop},
	rattle_client:push(StateData#state.client_pid, Noop),
	{next_state, processing, refresh_dc_timer(StateData)};

listening({heartbeat, ReplyChannel}, StateData) ->
	rattle_client:heartbeat(StateData#state.client_pid, ReplyChannel),
	{next_state, listening, refresh_heartbeat_timer(StateData)};

listening({push, Message}, StateData) ->
	NewState1 = append_to_queue(StateData, Message),
	NewState2 = refresh_flush_timer(NewState1),
	{next_state, buffering, NewState2};

listening(disconnect, StateData) ->
	{stop, normal, StateData}.

%%
%% BUFFERING state
%%
buffering(timeout, StateData) ->
	NewState1 = flush_queue(StateData),
	NewState2 = refresh_dc_timer(NewState1),
	{next_state, processing, NewState2};

buffering({heartbeat, ReplyChannel}, StateData) ->
	rattle_client:heartbeat(StateData#state.client_pid, ReplyChannel),
	{next_state, buffering, StateData};

buffering({push, Message}, StateData) ->
	{next_state, buffering, append_to_queue(StateData, Message)};

buffering(disconnect, StateData) ->
	{stop, normal, StateData}.

%% ===================================================================
%% Calbacks
%% ===================================================================
init({Sid, ClientSup}) ->
	lager:debug("Broker process initialized for sid ~s", [Sid]),

	{ok, ClientPid} = supervisor:start_child(ClientSup, [Sid, self()]),
	link(ClientPid),
	process_flag(trap_exit, true),

	DcTimer = gen_fsm:send_event_after(?DC_TIMEOUT, disconnect),
	{ok, transient, #state{
						   queue      = queue:new(),
						   dc_timer   = DcTimer,
						   client_pid = ClientPid
						  }}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

handle_info(Info, StateName, StateData) ->
	lager:debug("Broker received info: ~p", [Info]),
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	lager:debug("Broker process terminated").

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% ===================================================================
%% Internal functions
%% ===================================================================
append_to_queue(StateData, Message) ->
	NewQueue = queue:in(Message, StateData#state.queue),
	StateData#state{queue = NewQueue}.

flush_queue(StateData) ->
	rattle_client:push(StateData#state.client_pid, 
					   queue:to_list(StateData#state.queue)),
	StateData#state{queue = queue:new()}.

refresh_dc_timer(StateData) ->
	if 
		StateData#state.dc_timer == undefined -> ok;
		true -> gen_fsm:cancel_timer(StateData#state.dc_timer)
	end,
	StateData#state{dc_timer = 
					gen_fsm:send_event_after(?DC_TIMEOUT, disconnect)}.

cancel_dc_timer(StateData) ->
	gen_fsm:cancel_timer(StateData#state.dc_timer),
	StateData.

refresh_flush_timer(StateData) ->
	if 
		StateData#state.flush_timer == undefined -> ok;
		true -> gen_fsm:cancel_timer(StateData#state.flush_timer)
	end,
	StateData#state{flush_timer =
					gen_fsm:send_event_after(?FLUSH_TIMEOUT, timeout)}.

refresh_heartbeat_timer(StateData) ->
	if 
		StateData#state.flush_timer == undefined -> ok;
		true -> gen_fsm:cancel_timer(StateData#state.flush_timer)
	end,
	StateData#state{flush_timer =
					gen_fsm:send_event_after(?HEARTBEAT_TIMEOUT, timeout)}.