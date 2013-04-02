%%% -------------------------------------------------------------------
%%% @author Łukasz Lalik <lukasz.lalik@gmail.com>
%%%
%%% @doc 
%%% @end
%%% -------------------------------------------------------------------
-module(rattle_api_req_dispatcher).
-author('lukasz.lalik@gmail.com').

-export([start_link/0, http_loop/1]).

-include("internal_messages.hrl").

%% ===================================================================
%% Interface
%% ===================================================================

start_link() ->
	Loop = fun (Req) ->
			?MODULE:http_loop(Req)
		   end,

	mochiweb_http:start_link([
							  {name, rattle_api_req},
							  {loop, Loop},
							  {port, 9001}
							 ]).

http_loop(Request) ->
	"/" ++ Path = Request:get(path),
	PathParts = string:tokens(Path, "/"),
	Method = Request:get(method),

	case Method of
		Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			case PathParts of
				[] -> 
					rattle_utils:format_api_response(Request, 200, "Rattle API endpoint");

				["sessions"] ->
					Sids = rattle_router:session_list(),
					rattle_utils:format_api_response(Request, 200, mochijson2:encode(Sids));

				_ -> 
					error
			end;

		Method when Method =:= 'POST' ->
			case PathParts of
				[] ->
					rattle_utils:format_api_response(Request, 200, "Rattle API endpoint");

				% Temporary for demonstration purposes
				["all"] ->
					Sids = rattle_router:session_list(),
					Message = #imsg{level = socketio,
									type  = message,
									payload = Request:recv_body()},
					send_to_all(Sids, Message),
					rattle_utils:format_api_response(Request, 200, "OK");


				_ ->
					error
			end;

		_ ->
			error
	end.

%% ===================================================================
%% Internal functions
%% ===================================================================

send_to_all([], _) ->
	ok;
send_to_all([Sid | Rest], Message) ->
	LSid = binary_to_list(Sid),
	rattle_router:push(LSid, Message),
	send_to_all(Rest, Message).