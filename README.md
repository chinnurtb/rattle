# Rattle
======

Rattle is a lightweight Socket.io server written in Erlang. You can easily add custom message handling logic to it and you don't have to know how to write even single line in Erlang. 
Just start Rattle and integrate it with your application using simple HTTP rest API.

## First demo
======
* Clone dependencies `./rebar get-deps`
* Compile and start Rattle server (just type `./dev` when in rattle project directory)
* Open in browser example socket.io client. `client.html` from `test` directory
* You should get message `Connected to rattle!` in browser window
* Now send message to clients using API, execute bash script `.\push "Let's rattle"` from `test` directory
* Message should appear in browser :)