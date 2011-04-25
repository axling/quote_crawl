%%%-------------------------------------------------------------------
%%% File    : quote_crawl.erl
%%%-------------------------------------------------------------------
-module(quote_crawl).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case quote_crawl_sup:start_link() of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.    
