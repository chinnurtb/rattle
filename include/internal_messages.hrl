% Batch
-record(imsg_batch, {
					 level   = socketio,
					 payload = []
					}).

% Client-server internal message format
-record(imsg, {
			   level   	= socketio,
			   type    	= event,
			   id	 	= null,
			   endpoint	= null,
			   payload  = null
			  }).