-module(wicket).
-behaviour(gen_server).


-define(SERVER, ?MODULE).
-define(TIMEOUT, 300).

%% API
-export([start_link/0]).
-export([send/2]).
-export([send/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(message, {uri, params, method=get, return}).
-record(state, {queue=queue:new()}).

start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send(To, Text) ->
    {Service, Token, From, Mode} = get_credentials(),
    queue(From, To, Text, Service, Token, Mode).

send(From, To, Text) ->
    {Service, Token, _, Mode} = get_credentials(),
    queue(From, To, Text, Service, Token, Mode).

init([]) ->
	{ok, ServiceName} = application:get_env(wicket, service),
	{ok, Service} = file:consult(filename:join("priv", ServiceName)++".commands"),
	dict_cache:set_value(service, Service),
	{ok, #state{}, ?TIMEOUT}.

handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State, ?TIMEOUT}.

handle_cast({queue, Message}, State=#state{queue=Queue}) ->
    NewQueue = queue:in(Message, Queue),
    NewState = State#state{queue=NewQueue},
    {noreply, NewState, ?TIMEOUT}.

handle_info(timeout, State = #state{queue=Queue}) ->
    NewQueue = case queue:out(Queue) of
        {{value, Message}, NQ} ->
            spawn_send(Message),
            NQ;
        {empty, Queue} ->
            Queue
    end,
    {noreply, State#state{queue=NewQueue}, ?TIMEOUT}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

get_credentials() ->
    {ok, Token} = application:get_env(wicket, auth_token),
    {ok, From} = application:get_env(wicket, from),
    {ok, Mode} = application:get_env(wicket, mode),
	Service=dict_cache:get_value(service),
    {Service, Token, From, Mode}.

queue(From, To, Text, Service, Token, Mode=plain)  ->
    spawn(fun() ->
        Parts = split_text(Text, 160),
        Send=proplists:get_value(send, Service),
        Host=proplists:get_value(host, Send),
        Method=proplists:get_value(method, Send),
        Return=proplists:get_value(return, Send),
        Params=proplists:get_value(params, Send),
        Path=string:join([Host, proplists:get_value(uri, Send)], "/"),
        lists:foreach(fun(Part) ->
			Vals=process_params(Params, [{authname,Token}, {number, To}, {message, Text}, {sender, From}]),
            Message = #message{uri=Path, method=Method, params=Vals, return=Return},
            queue(Message)
%            ,timer:sleep(5000)
        end, Parts)
    end).

process_params(Params, Values) ->
	lists:foldl(fun(Param,Acc) ->  
		Rt=case Param of
			{Name, Code, mandatory, Default} -> set_val(Code, mandatory, proplists:get_value(Name, Values, Default));
			{Name, Code, mandatory} -> set_val(Code, mandatory, proplists:get_value(Name, Values, undefined));
			{Name, Code, nonmandatory, Default} -> set_val(Code, nonmandatory, proplists:get_value(Name, Values, Default));
			{Name, Code, nonmandatory} -> set_val(Code, nonmandatory, proplists:get_value(Name, Values, undefined))
		end,
		case Rt of
			{} -> Acc;
			_ -> Acc ++ [Rt]
		end
	end, [], Params)
.

set_val(Code, mandatory, Value) when Value /=undefined ->
	{Code, Value}
;

set_val(Code, nonmandatory, Value) when Value /=undefined ->
{Code, Value}
;

set_val(Code, nonmandatory, Value) ->
{}
.


split_text(Text, Chars) when length(Text) =< Chars ->
    [Text];
split_text(Text, Chars) when length(Text) =< Chars*9 ->
    Parts = sigma:ceiling(length(Text) / (Chars - 6)),
    split_text(Text, Chars, 1, Parts).

split_text(Text, Chars, Part, Parts) when length(Text) < Chars-6 ->
    [format_part(Text, Part, Parts)];
split_text(Text, Chars, Part, Parts) ->
    {This, Rest} = lists:split(Chars - 6, Text),
    [format_part(This, Part, Parts) | split_text(Rest, Chars, Part+1, Parts)].

format_part(Text, Part, Parts) ->
    lists:flatten(io_lib:format("(~p/~p) ~s", [Part, Parts, Text])).
    

queue(Message = #message{}) ->
    gen_server:cast(?MODULE, {queue, Message}).

spawn_send(Message) ->
    spawn(fun() ->
        try
            real_send(Message)
        catch E:T ->
            error_logger:error_msg("~p:~p~n~p~n", [E, T, erlang:get_stacktrace()])
        end
    end).

real_send(#message{uri=Path, method=Method, params=Vals, return=Return}) ->

    
    RQ=to_qs(Vals),
	
    Request = {
        Path ++ "?" ++RQ,
        []
    },
    HTTPOpts=[
		{timeout, 60000}
    ],
	Opts = [
        {full_result, false}
    ],

    case httpc:request(Method, Request, HTTPOpts, Opts) of
        {ok, {201, Result}} ->
            {ok, Result};
        {ok, {Code, ErrorJson}} ->
            error_logger:error_msg("There was an error sending message. HTTP Response Code: ~p~nError Response: ~p", [Code, ErrorJson])
    end.

to_qs(List) ->
    KVs = [escape_kv(KV) || KV <- List],
    binary_to_list(iolist_to_binary(string:join(KVs, "&"))).

escape_kv({Key, Val}) ->
[edoc_lib:escape_uri(Key), "=", edoc_lib:escape_uri(Val)].
