%%%-------------------------------------------------------------------
%%% File    : quote_crawl.erl
%%%-------------------------------------------------------------------
-module(quote_crawl).

-export([init/0, start/2, stop/1, start_link/0, get/1, update/1,
	 get_stocks_dict/0]).

-include("quote_crawl.hrl").

start_link() ->
    spawn_link(?MODULE, init, []).

get(Stock) ->
    ?MODULE ! {get, Stock, self()},
    receive 
	{get_reply, Value} ->
	    Value
    end.

update(Stock) ->
    ?MODULE ! {update, Stock, self()},
    receive 
	{update_reply, Value} ->
	    Value
    end.

get_stocks_dict() ->
    ?MODULE ! {get_stocks_dict, self()},
    receive 
	{get_stocks_dict_reply, Value} ->
	    Value
    end.

init() ->
    register(?MODULE, self()),
    inets:start(),
    loop(dict:new()).

loop(StocksDict) ->
    receive 
	{update, Stock, From} ->
	    Pages = [qc_http:get_text_tv(Page) || Page <- lists:seq(203, 220)],
	    ParsedResults = [ qc_text_tv:parse_page(Page) || Page <- Pages],
	    
	    NewStocksDict = update_dict(lists:flatten(ParsedResults), 
					StocksDict),	    
	    From ! {update_reply, dict:fetch(Stock, NewStocksDict)},
	    loop(NewStocksDict);
	{get, Stock, From} ->
	    case dict:is_key(Stock, StocksDict) of
		false ->
		    From ! {get_reply, not_existing},
		    loop(StocksDict);
		true ->
		    Val = dict:fetch(Stock, StocksDict),
		    From ! {get_reply, {ok, Val}},
		    loop(StocksDict)
	    end;
	{get_stocks_dict, From} ->
	    From ! {get_stocks_dict_reply, StocksDict},
	    loop(StocksDict)
    end.

update_dict([], StocksDict) ->
    StocksDict;
update_dict([Result | Rest], StocksDict) ->
    NewStocksDict = dict:store(Result#stock.name, Result, StocksDict),
    update_dict(Rest, NewStocksDict).
	    

start(_Type, _StartArgs) ->
    case quote_crawl_sup:start_link() of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.    
