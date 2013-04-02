%%% -------------------------------------------------------------------
%%% @author Łukasz Lalik <lukasz.lalik@gmail.com>
%%%
%%% @doc 
%%% @end
%%% -------------------------------------------------------------------
-module(rattle_client).
-author('lukasz.lalik@gmail.com').

-behavior(gen_fsm).

-export([start_link/2, connect/2, heartbeat/2, push/2]).
-export([disconnected/2, connected/2]).
-export([init/1, handle_event/3, handle_sync_event/4,
		 handle_info/3, terminate/3, code_change/4]).

-include("internal_messages.hrl").

-record(state, {sid, broker_pid, reply_channel}).

%% ===================================================================
%% Interface
%% ===================================================================
start_link(Sid, BrokerPid) ->
	gen_fsm:start_link(?MODULE, {Sid, BrokerPid}, []).

connect(Pid, ReplyChannel) ->
	gen_fsm:send_event(Pid, {connect, ReplyChannel}).

heartbeat(Pid, ReplyChannel) ->
	gen_fsm:send_all_state_event(Pid, {heartbeat, ReplyChannel}).

push(Pid, Message) ->
	gen_fsm:send_all_state_event(Pid, {push, Message}).

%% ===================================================================
%% States
%% ===================================================================

disconnected({connect, ReplyChannel}, StateData) ->
	NewState = StateData#state{reply_channel = ReplyChannel},
	Message = #imsg{level = socketio,
					type  = connected},
	ReplyChannel(Message),
	{next_state, connected, NewState}.

connected(_Event, StateData) ->
	{next_state, connected, StateData}.

%% ===================================================================
%% Callbacks
%% ===================================================================
init({Sid, BrokerPid}) ->
	process_flag(trap_exit, true),
	{ok, disconnected, #state{
							  sid = Sid,
							  broker_pid = BrokerPid
							 }}.

handle_event({heartbeat, ReplyChannel}, StateName, StateData) ->
	NewState = StateData#state{reply_channel = ReplyChannel},
	{next_state, StateName, NewState};

handle_event({push, Message}, StateName, StateData) when is_list(Message) ->
	ReplyChannel = StateData#state.reply_channel,
	ReplyChannel(#imsg_batch{level = socketio,
							 payload = Message}),
	{next_state, StateName, StateData};
handle_event({push, Message}, StateName, StateData) ->
	ReplyChannel = StateData#state.reply_channel,
	ReplyChannel(Message),
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

handle_info({'EXIT', Pid, _Reason}, _StateName, StateData) 
			when Pid =:= StateData#state.broker_pid ->
	{stop, normal, StateData};

handle_info(Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.