% Input internal message
-record(in_imsg, {}).


% Batch
-record(out_imsg_batch, {
						 level   = socketio,
						 payload = []
						}).

% Output internal message
-record(out_imsg, {
				   level   = socketio,
				   type    = event,
				   payload = ""
				  }).