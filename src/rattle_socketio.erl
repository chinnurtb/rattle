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
-define(EVENT, "5:::~s").
-define(ACK, "6:::~s[~s]").
-define(NOOP, "8::").

-define(XHR_FRAME, unicode:characters_to_binary([16#fffd], utf16)).

-define(TRANSPORTS, "xhr-polling").

-record(signal, {type, id, endpoint, payload}).

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
		M when M#imsg.type == handshake ->
			lists:flatten(io_lib:format(?HANDSHAKE, [M#imsg.payload, 30, 20, ?TRANSPORTS]));

		M when M#imsg.type == connected ->
			?CONNECTED;

		M when M#imsg.type == noop ->
			?NOOP;

		M when M#imsg.type == message ->
			lists:flatten(io_lib:format(?MESSAGE, [M#imsg.payload]));

		_ ->
			ok
	end.

%% ===================================================================
%% Private functions
%% ===================================================================

decode_signal(Signal) ->	
	HeaderLength = decode_header(Signal, 0, 0),
	<<Header:HeaderLength/binary, Payload/binary>> = Signal,
	[Type, Id, Endpoint | _] = binary:split(Header, <<":">>, [global]),
	
	#signal{
			type 		= Type,
			id 			= binary_to_list(Id),
			endpoint 	= Endpoint,
			payload		= Payload
		   }.

decode_header(<<"">>, Length, 3) ->
	Length;
decode_header(<<"">>, _, _) ->
	error;
decode_header(Signal, Length, 3) ->
	Length;
decode_header(<<$:, Rest/binary>>, Length, Occurences)->
	decode_header(Rest, Length + 1, Occurences + 1);
decode_header(<<_, Rest/binary>>, Length, Occurences) ->
	decode_header(Rest, Length + 1, Occurences).
