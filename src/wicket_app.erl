-module(wicket_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	dict_cache:start_link(),
	dict_cache:init(),
    wicket_sup:start_link().

stop(_State) ->
ok.
