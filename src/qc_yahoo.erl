%%%-------------------------------------------------------------------
%%% File    : qc_text_tv.erl
%%%-------------------------------------------------------------------
-module(qc_yahoo).

-include("quote_crawl.hrl").

-export([get/2,
	 request/2]).

-define(BASE, "http://finance.yahoo.com/d/quotes.csv").

-spec get(Quotes :: [atom()], Types :: [atom()]) ->
		 list() | {error, Reason :: any()}.
get(Quotes, Types) when is_list(Quotes), is_list(Types) ->
    YahooQuotes = string:join([atom_to_list(Quote) || Quote <- Quotes], "+"),
    YahooTypes = lists:concat([convert_type(Type) || Type <- Types]),
    case request(YahooQuotes, YahooTypes) of
	{error, Reason} ->
	    {error, Reason};
	Body ->
	    SplitBody = re:split(Body, "\\r\\n", [{return, list}, trim]),
	    parse_request_body(SplitBody, Quotes, Types, [])
    end.	       

-spec request(Quote :: string(), Types :: string()) ->
		     Body :: string() | {error, Reason :: any()}.
request(Quotes, Types) when is_list(Quotes), is_list(Types) ->
    inets:start(),
    case httpc:request(?BASE ++ "?s=" ++ Quotes ++ "&f=" ++ Types) of
	{ok, {_, _, Body}} ->  
	    inets:stop(),
	    Body;
	{ok, {_, Body}} ->
	    inets:stop(),
	    Body;
	{error, Reason} ->
	    inets:stop(),
	    {error, Reason}
    end.
    
	 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_request_body([], [], Types, AccResult) ->
    lists:reverse(AccResult);
parse_request_body([QuotePart | Body], [Quote | Quotes], Types, AccResult) ->
    SplitTypes = re:split(QuotePart, ",", [{return, list}, trim]),
    TypeResultList = 
	lists:map(
	  fun({Type, ParsedResult}) ->
		  parse_result(Type, ParsedResult)
	  end, lists:zip(Types, SplitTypes)),
    parse_request_body(Body, Quotes, Types, [{Quote, TypeResultList} | 
					     AccResult]).
    
parse_result(ask, Result) ->
    {ask, list_to_number(Result)};
parse_result(Else, Result) ->
    {Else, Result}.

convert_type(ask) ->
    "a";
convert_type(Else) ->
    Else.

list_to_number(Number) when is_list(Number) ->
    try 
	list_to_float(Number)
    catch _:_ ->
	    list_to_integer(Number)
    end.
