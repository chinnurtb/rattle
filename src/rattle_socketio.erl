%%% -------------------------------------------------------------------
%%% @author Łukasz Lalik <lukasz.lalik@gmail.com>
%%%
%%% @doc 
%%% @end
%%% -------------------------------------------------------------------
-module(rattle_socketio).
-author('lukasz.lalik@gmail.com').

-export([wrap_batch_message/1, wrap_message/1]).

-include("internal_messages.hrl").

-define(HANDSHAKE, "~s:~p:~p:~s").
-define(CONNECTED, "1::").
-define(MESSAGE, "3:::~s").
-define(NOOP, "8::").

-define(XHR_FRAME, unicode:characters_to_binary([16#fffd], utf16)).

-define(TRANSPORTS, "xhr-polling").
%% ===================================================================
%% Interface
%% ===================================================================



wrap_batch_message([]) ->
	"";
wrap_batch_message([Message | Rest]) ->
	Msg = wrap_message(Message),
	Len = string:len(Msg),
	Frame = io_lib:format("~s~p~s~s", [?XHR_FRAME, Len, ?XHR_FRAME, Msg]),
	string:concat(Frame, wrap_batch_message(Rest)).

wrap_message(IMessage) ->
	case IMessage of
		M when M#out_imsg.type == handshake ->
			lists:flatten(io_lib:format(?HANDSHAKE, [M#out_imsg.payload, 30, 20, ?TRANSPORTS]));

		M when M#out_imsg.type == connected ->
			?CONNECTED;

		M when M#out_imsg.type == noop ->
			?NOOP;

		M when M#out_imsg.type == message ->
			lists:flatten(io_lib:format(?MESSAGE, [M#out_imsg.payload]));

		_ ->
			ok
	end.