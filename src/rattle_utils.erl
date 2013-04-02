%%% -------------------------------------------------------------------
%%% @author Łukasz Lalik <lukasz.lalik@gmail.com>
%%%
%%% @doc 
%%% @end
%%% -------------------------------------------------------------------
-module(rattle_utils).
-author('lukasz.lalik@gmail.com').

-export([generate_sid/0, format_api_response/3, format_xhr_response/3, build_xhr_reply_channel/0]).

%% ===================================================================
%% Interface
%% ===================================================================

generate_sid() ->
	{Mega, Sec, Micro} = erlang:now(),
	Rand = random:uniform(999999),
	Sid = integer_to_list(Rand)   ++  
		  integer_to_list(Mega)   ++ 
		  integer_to_list(Sec)    ++
		  integer_to_list(Micro).

format_api_response(Req, Code, Body) ->
	Req:respond({Code, [
						{"Content-Type", "application/json; cherset=UTF-8"},
				   		{"Server", "Rattle API server"}
					   ], Body}).

format_xhr_response(Req, Code, Body) ->
	Req:respond({Code, [
						{"Origin", "*.*"},
						{"Content-Type", "text/plain; cherset=UTF-8"},
						{"Access-Control-Allow-Origin", "null"},
				   		{"Access-Control-Allow-Headers", "Content-Type"},
				   		{"Access-Control-Allow-Credentials", "true"},
				   		{"Server", "Rattle socket.io server"}
					   ], Body}).

build_xhr_reply_channel() ->
	ReceiverPid = self(),
	fun(Message) ->
		erlang:send(ReceiverPid, Message)
	end.