%%%-------------------------------------------------------------------
%%% File    : qc_text_tv.erl
%%%-------------------------------------------------------------------
-module(qc_yahoo).

-include("quote_crawl.hrl").

-export([request/2]).

-define(BASE, "http://finance.yahoo.com/d/quotes.csv").

request(Quote, Type) when is_list(Quote), is_list(Type) ->
    inets:start(),
    case httpc:request(?BASE ++ "?s=" ++ Quote ++ "&f=" ++ Type) of
	{ok, {_, _, Body}} ->  
	    Body;
	{ok, {_, Body}} ->
	    Body;
	{error, Reason} ->
	    {error, Reason}
    end.
    
	 
