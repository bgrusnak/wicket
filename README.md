Wicket
=======
The Unified Erlang SMS gateway library

Supported services
----------

msg91.com - https://msg91.com
Other are coming soon...

How to use
----------

Set auth_token and from in cofiguration

```erlang
wicket:start_link().

wicket:send(To, Text) % sending Text to the comma-separated numbers in To
wicket:send(From, To, Text) % sending Text with From caller ID to the comma-separated numbers in To

```
