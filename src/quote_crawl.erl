%%%-------------------------------------------------------------------
%%% File    : quote_crawl.erl
%%%-------------------------------------------------------------------
-module(quote_crawl).

-export([init/0, start/2, stop/1, start_link/0, update/1]).

-include("quote_crawl.hrl").

start_link() ->
    spawn_link(?MODULE, init, []).

update(Stock) ->
    ?MODULE ! {update, Stock, self()},
    receive 
	{update_reply, Value} ->
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
	    ParsedResults = [qc_text_tv:parse_page(Page) || Page <- Pages],
	    NewStocksDict = update_dict(ParsedResults, StocksDict),
	    From ! {update_reply, dict:fetch(Stock, NewStocksDict)},
	    loop(NewStocksDict)
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
