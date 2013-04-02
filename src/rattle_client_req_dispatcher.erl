%%% -------------------------------------------------------------------
%%% @author Łukasz Lalik <lukasz.lalik@gmail.com>
%%%
%%% @doc 
%%% @end
%%% -------------------------------------------------------------------
-module(rattle_client_req_dispatcher).
-author('lukasz.lalik@gmail.com').

-export([start_link/0, http_loop/1, xhr_respond/3]).

-include("internal_messages.hrl").

%% ===================================================================
%% Interface
%% ===================================================================

start_link() ->
	Loop = fun (Req) ->
			?MODULE:http_loop(Req)
		   end,

	mochiweb_http:start_link([
							  {name, rattle_client_req},
							  {loop, Loop},
							  {port, 9000}
							 ]).

http_loop(Request) ->
	"/" ++ Path = Request:get(path),
	PathParts = string:tokens(Path, "/"),
	Method = Request:get(method),

	case Method of
		Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			case PathParts of
				[] -> 
					rattle_utils:format_xhr_response(Request, 200, "Welcome to Rattle!");

				["socket.io", "1", "xhr-polling", Sid | _] ->
					Reentry = mochiweb_http:reentry({?MODULE, http_loop}),
					ReplyChannel = rattle_utils:build_xhr_reply_channel(),
					rattle_router:heartbeat(Sid, ReplyChannel),
					DcTimer = erlang:send_after(30000, self(), disconnect),
					proc_lib:hibernate(?MODULE, xhr_respond, [Request, Reentry, DcTimer]);

				["socket.io", "1"] ->
					case rattle_router:login() of
						Sid when is_list(Sid) ->
							Message = #imsg{level = socketio,
											type  = handshake,
											payload = Sid},
							Response = rattle_socketio:wrap_message(Message),
							rattle_utils:format_xhr_response(Request, 200, Response);

						_ ->
							error
					end;

				_ -> 
					error
			end;

		Method when Method =:= 'POST' ->
			case PathParts of
				["socket.io", "1", Transport, Sid | _] ->
					event;

				_ ->
					error
			end;

		_ ->
			error
	end.

xhr_respond(Request, Reentry, DcTimer) ->
	erlang:cancel_timer(DcTimer),
	receive 
		R when R#imsg.level == server ->
			rattle_utils:format_xhr_response(Request, 500, "Internal error"),
			exit(normal);

		R when R#imsg.level == socketio ->
			Message = rattle_socketio:wrap_message(R),
			rattle_utils:format_xhr_response(Request, 200, Message);

		% @TODO Think about it. Level for whole batch?
		R when R#imsg_batch.level == socketio ->
			Message = rattle_socketio:wrap_batch_message(R#imsg_batch.payload),
			rattle_utils:format_xhr_response(Request, 200, Message);

		_ ->
			lager:debug("Kill HTTP process after timeout"),
			rattle_utils:format_xhr_response(Request, 404, "Channel closed"),
			exit(normal)
	end,
	Reentry(Request).

%% ===================================================================
%% Internal functions
%% ===================================================================