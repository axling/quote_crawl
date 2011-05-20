%%%-------------------------------------------------------------------
%%% File    : qc_text_tv.erl
%%%-------------------------------------------------------------------
-module(qc_text_tv).

-include("quote_crawl.hrl").

-export([parse_page/1]).

-define(FALLING_STOCK_WITH_VOLUME,
	"<span\\s+class='C'>(?<diff>(?<number>\\d+.\\d+|\\d+))\\s+"
	"(?<buy>\\k<number>)\\s+(?<sell>\\k<number>)"
	"(?<event>sp|xr|xd|xs|rh|th|ts|sr|po|ob|tr|bt|os|sdb|ss""|\\s+)"
	"(?<name>[A-Wa-w]+|BIOPto 1)\\s*(?<latest>\\k<number>)\\s+"
	"(?<volume>\\d+)</span>").
-define(FALLING_STOCK_WITH_HIGH_LOW,
	"<span\\s+class='C'>(?<diff>(?<number>\\d+.\\d+|\\d+))\\s+"
	"(?<buy>\\k<number>)\\s+(?<sell>\\k<number>)"
	"(?<event>sp|xr|xd|xs|rh|th|ts|sr|po|ob|tr|bt|os|sdb|ss""|\\s+)"
	"(?<name>[A-Wa-w]+|BIOPto 1)\\s*(?<latest>\\k<number>)\\s+"
	"(?<high>\\k<number>)\\s+(?<low>\\k<number>)</span>").
-define(RISING_STOCK_WITH_VOLUME,
	"<span\\s+class='Y'>(?<diff>(?<number>\\d+.\\d+|\\d+))\\s+"
	"(?<buy>\\k<number>)\\s+(?<sell>\\k<number>)"
	"(?<event>sp|xr|xd|xs|rh|th|ts|sr|po|ob|tr|bt|os|sdb|ss""|\\s+)"
	"(?<name>[A-Wa-w]+|BIOPto 1)\\s*(?<latest>\\k<number>)\\s+"
	"(?<volume>\\d+)</span>").
-define(RISING_STOCK_WITH_HIGH_LOW,
	"<span\\s+class='Y'>(?<diff>(?<number>\\d+.\\d+|\\d+))\\s+"
	"(?<buy>\\k<number>)\\s+(?<sell>\\k<number>)"
	"(?<event>sp|xr|xd|xs|rh|th|ts|sr|po|ob|tr|bt|os|sdb|ss""|\\s+)"
	"(?<name>[A-Wa-w]+|BIOPto 1)\\s*(?<latest>\\k<number>)\\s+"
	"(?<high>\\k<number>)\\s+(?<low>\\k<number>)</span>").
-define(UNCHANGED_STOCK_WITH_VOLUME,
	"<span\\s+class='W'>.*"
	"(?<event>sp|xr|xd|xs|rh|th|ts|sr|po|ob|tr|bt|os|sdb|ss""|\\s+)"
	"(?<name>[A-Wa-w]+|BIOPto 1)\\s*(?<latest>\\k<number>)\\s+"
	"(?<volume>\\d+)</span>").

-define(SPAN, "<span\\s+class='[YWC]'>.*</span>").

is_match(String) ->
    case re:run(String, ?SPAN) of
	nomatch ->
	    false;
	_Else ->
	    true
    end.

split(String, RegExp) ->
    re:split(String, RegExp, [{return, list}, trim]).

parse_page(Page) ->
    StringList = split(Page, "\n"),
    NewStringList = 
	lists:filter(
	  fun(String) ->
		  is_match(String)
	  end, StringList),
    ResultList = 
	lists:map(
	  fun(String) ->
		  case match_falling_stock_with_volume(String) of
		      nomatch ->
			  io:format("No match found for:~n~p~n", [String]),
			  undefined;
		      #stock{} = Stock ->
			  Stock
		  end
	  end, NewStringList),
    lists:filter(fun(undefined)-> false; (_) -> true end,
		 merge_entrys(ResultList)).


match_falling_stock_with_volume(String) ->
    case re:run(String, ?FALLING_STOCK_WITH_VOLUME, 
		[{capture, [diff, buy, sell, event, name, latest, volume],
		  list}]) of
	{match, [_Diff, Buy, Sell, _Event, Name, _Latest, Volume]} ->
	    #stock{name=Name, buy=Buy, sell=Sell, volume=Volume};
	nomatch ->
	    match_falling_stock_with_high_low(String)
    end.
match_falling_stock_with_high_low(String) ->
    case re:run(String, ?FALLING_STOCK_WITH_HIGH_LOW,
		[{capture, [diff, event, name, latest, high, low],
		  list}]) of
	{match, [_Diff, _Event, Name, _Latest, High, Low]} ->
	    #stock{name=Name, highest=High, lowest=Low};
	nomatch ->
	    match_rising_stock_with_volume(String)
    end.

match_rising_stock_with_volume(String) ->
    case re:run(String, ?RISING_STOCK_WITH_VOLUME,
		[{capture, [diff, buy, sell, event, name, latest, volume],
		  list}]) of
	{match, [_Diff, Buy, Sell, _Event, Name, _Latest, Volume]} ->
	    #stock{name=Name, buy=Buy, sell=Sell, volume=Volume};
	nomatch ->
	    match_rising_stock_with_high_low(String)
    end.

match_rising_stock_with_high_low(String) ->
    case re:run(String, ?RISING_STOCK_WITH_HIGH_LOW,
		[{capture, [diff, event, name, latest, high, low],
		  list}]) of
	{match, [_Diff, _Event, Name, _Latest, High, Low]} ->
	    #stock{name=Name, highest=High, lowest=Low};
	nomatch ->
	    match_unchanged_stock_with_volume(String)
    end.

match_unchanged_stock_with_volume(String) ->
    case re:run(String, ?UNCHANGED_STOCK_WITH_VOLUME,
		[{capture, [name, latest, volume],
		  list}]) of
	{match, [Name, _Latest, Volume]} ->
	    #stock{name=Name, volume=Volume};
	nomatch ->
	    nomatch
    end.    

merge_entrys(ResultList) ->
    NewList = 
	lists:foldl(
	  fun(#stock{name=Name} = Entry, AccIn) ->
		  case lists:keyfind(Name, 1, AccIn) of
		      {Name, List} ->
			  lists:keyreplace(Name, 1, AccIn, 
					   {Name, [Entry | List]});
		      false ->
			  [{Name, [Entry]} | AccIn]
		  end
	  end, [], ResultList),
    lists:map(fun({_Name, [Stock1, Stock2]}) -> 
		      merge_record(Stock1, Stock2, size(Stock1));
		 ({_Name, [_Stock1]}) ->
		      undefined
	      end, NewList).

merge_record(Stock1, _Stock2, 0) ->
    Stock1;
merge_record(Stock1, Stock2, Index) ->
    if 
	element(Index, Stock1) == element(Index, Stock2) ->
	    merge_record(Stock1, Stock2, Index - 1);
	(element(Index, Stock1) == undefined) and 
	(element(Index, Stock2) /= undefined) ->
	    NewStock1 = setelement(Index, Stock1, element(Index, Stock2)),
	    merge_record(NewStock1, Stock2, Index - 1);
	true ->
	    merge_record(Stock1, Stock2, Index - 1)
    end.

list_to_number(Number) ->	
    case catch list_to_float(Number) of
	{'EXIT',_} ->
	    list_to_integer(Number);
	Else ->
	    Else
    end.
