%%%-------------------------------------------------------------------
%%% File    : qc_text_tv.erl
%%%-------------------------------------------------------------------
-module(qc_text_tv).

-include("quote_crawl.hrl").

-export([parse_page/1]).

-define(GENERAL_SPAN, "<span\\s*class=\"[YCW]\">[^<]+</span>").
%%-define(GENERAL_SPAN, "\n").
-define(FLOAT, "[0-9]+\\.[0-9]+").
-define(NUMBER, "(?:" ++ ?FLOAT ++ "|\\d+)").
-define(NAME, "[A-Wa-w]+").
-define(STOCK_TYPE1, "<span\\s*class=\"[YC]\">\\s*(?<diff>" ++ ?NUMBER ++
	")\\s+(?<buy1>" ++ ?NUMBER ++ ")\\s+(?<sell1>" ++ ?NUMBER ++ 
	")\\s*(?<name1>" ++ ?NAME ++ ")\\s*(?<latest1>" ++ ?NUMBER ++ 
	")\\s+(?<number1>" ++ ?NUMBER ++ ")\\s*</span>").
-define(STOCK_TYPE2, "<span\\s*class=\"W\">\\s*(?<buy2>" ++ 
	?NUMBER ++")\\s+(?<sell2>" ++ ?NUMBER ++ 
	")\\s*(?<name2>" ++ ?NAME ++ ")\\s*(?<latest2>" ++ ?NUMBER ++ 
	")\\s+(?<number3>" ++ ?NUMBER ++ ")\\s*</span>").
-define(STOCK_TYPE3, "<span\\s*class=\"[YC]\">\\s*" ++ ?NUMBER ++
	"\\s+" ++ ?NUMBER ++ "\\s+" ++ ?NUMBER ++ 
	"\\s*(?<name3>" ++ ?NAME ++ ")\\s*" ++ ?NUMBER ++ 
	"\\s+(?<highest3>" ++ ?NUMBER ++ ")\\s+(?<lowest3>" ++ 
	?NUMBER ++ ")\\s*</span>").
-define(STOCK_TYPE4, "<span\\s*class=\"W\">\\s*"
	++ ?NUMBER ++ "\\s+" ++ ?NUMBER ++ 
	"\\s*(?<name4>" ++ ?NAME ++ ")\\s*" ++ ?NUMBER ++ 
	"\\s+(?<highest4>" ++ ?NUMBER ++ ")\\s+(?<lowest4>" ++ 
	?NUMBER ++ ")\\s*</span>").

-define(STOCKS, "(" ++ ?STOCK_TYPE1 ++ "|" ++ ?STOCK_TYPE2 ++ "|" 
	++ ?STOCK_TYPE3 ++ "|" ++ ?STOCK_TYPE4 ++ ")").

is_match(String, RegExp) ->
    case re:run(String, RegExp) of
	nomatch ->
	    false;
	_Else ->
	    true
    end.

match(String, RegExp) ->
    re:run(String, RegExp, [{capture, all_but_first, list}]).

split(String, RegExp) ->
    re:split(String, RegExp,[{return, list}, trim]).

parse_page(Page) ->
    StringList = split(Page, "\n"),
    NewStringList = lists:filter(
 		      fun(String) ->
 			      is_match(String,?STOCKS)
 		      end, StringList),
    ResultList = 
	lists:map(
	  fun(String) ->
		  case match_type1(String) of
		      {error, Reason} ->
			  {error, Reason};
		      #stock{} = Stock ->
			  Stock
		  end
	  end, NewStringList),
    lists:filter(fun(undefined)-> false; (_) -> true end,
		 merge_entrys(ResultList)).


match_type1(String) ->
    case match(String, ?STOCK_TYPE1) of
	{match, [_Diff, Buy, Sell, Name, _Latest, Number]} ->
	    #stock{buy=list_to_number(Buy), sell=list_to_number(Sell), 
		   name=Name, volume=list_to_integer(Number)};
	nomatch ->
	    match_type2(String)
    end.

match_type2(String) ->
    case match(String, ?STOCK_TYPE2) of
	{match, [Buy, Sell, Name, _Latest, Number]} ->
	    #stock{buy=list_to_number(Buy), sell=list_to_number(Sell), 
		   name=Name, volume=list_to_integer(Number)};
	nomatch ->
	    match_type3(String)
    end.

match_type3(String) ->
    case match(String, ?STOCK_TYPE3) of
	{match, [Name, Highest, Lowest]} ->
	    #stock{name=Name, highest=list_to_number(Highest), lowest=list_to_number(Lowest)};
	nomatch ->
	    match_type4(String)
    end.

match_type4(String) ->
    case match(String, ?STOCK_TYPE4) of
	{match, [Name, Highest, Lowest]} ->
	    #stock{name=Name, highest=list_to_number(Highest), lowest=list_to_number(Lowest)};
	nomatch ->
	    match_type4(String)
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
		 ({_Name, [Stock1]}) ->
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
