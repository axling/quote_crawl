-module(qc_omx).

-export([parse_page/1, split/2, row_re/0, download_stock_data/3]).

-define(FLOAT, "[0-9]+\\,[0-9]+").
-define(NUMBER, "(?:" ++ ?FLOAT ++ "|\\d+)").
-define(AVGNUMBER, "(?:" ++ ?FLOAT ++ "|\\d+|\\s*)").
-define(NAME, "\\S+").

-define(DATE, "\\d{4}-\\d{2}-\\d{2}").
-define(ROW, 
	"<tr\\s+id=\"historicalTable-\">\\s*<td>(?<date>" ++ ?DATE ++
	")</td>\\s+<td>(?<max>"++ ?NUMBER ++ ")</td>\\s+" ++
	"<td>(?<min>" ++ ?NUMBER ++ ")</td>\\s*<td>(?<closing>" ++ ?NUMBER ++
	")</td>\\s+<td>(?<average>" ++ ?AVGNUMBER ++ ")</td>\\s+" ++ 
	"<td>(?<volume>[^<]+)</td>\\s+<td>(?<turnover>[^<]+)</td>\\s+<td>"
	"(?<completions>[^<]+)</td>").

download_stock_data(Instrument, StartDate, EndDate) ->
    StartDateString = convert_date_e_s(StartDate),
    EndDateString = convert_date_e_s(EndDate),
    
    Request = 
	"<post>\n"
	"<param name=\"SubSystem\" value=\"History\"/>\n"
	"<param name=\"Action\" value=\"GetDataSeries\"/>\n"
	"<param name=\"AppendIntraDay\" value=\"no\"/>\n"
	"<param name=\"Instrument\" value=\"" ++ Instrument ++ "\"/>\n"
	"<param name=\"FromDate\" value=\"" ++ StartDateString ++ "\"/>\n"
	"<param name=\"ToDate\" value=\"" ++ EndDateString ++ "\"/>\n"
	"<param name=\"hi__a\" value=\"0,1,2,4,21,8,10,11,12,9\"/>\n"
	"<param name=\"ext_xslt\" value=\"test/hi_table.xsl\"/>\n"
	"<param name=\"ext_xslt_options\" value=\",undefined,\"/>\n"
	"<param name=\"ext_xslt_lang\" value=\"sv\"/>\n"
	"<param name=\"ext_xslt_hiddenattrs\" value=\",ip,iv,\"/>\n"
	"<param name=\"ext_xslt_tableId\" value=\"historicalTable\"/>\n"
	"</post>",
    
    UrlEncodedReq = edoc_lib:escape_uri(Request),
    {ok, {_, _, Result}} = 
	httpc:request(
	  post, 
	  {"http://www.nasdaqomxnordic.com/webproxy/DataFeedProxy.aspx",
	   [], "application/x-www-form-urlencoded;charset=UTF-8", 
	   "xmlquery=" ++ UrlEncodedReq}, 
	  [], []),
    Result.

parse_page(Page) ->
    StringList = split(Page, "</tr>"),
    NewStringList = lists:filter(
 		      fun(String) ->
 			      is_match(String, ?ROW)
 		      end, StringList),
    ResultList = 
	lists:map(
	  fun(String) ->
		  case match_type(String) of
		      nomatch ->
			  nomatch;
		      Entry ->
			  Entry
		  end
	  end, NewStringList),
    lists:filter(fun(undefined) -> false; (_Else) -> true end, ResultList).

row_re()->
    ?ROW.

split(String, RegExp) ->
    re:split(String, RegExp,[{return, list}]).

match_type(String) ->
    case match(String, ?ROW) of
	{match, [Date, Max, Min, Closing, Average, Volume, TurnOver, Completions]} ->
	    {convert_date_s_e(Date), 
 	     to_number(Max), 
 	     to_number(Min), 
 	     to_number(Closing), 
 	     to_number(Average), 
 	     to_integer(Volume), 
 	     to_integer(TurnOver), 
 	     to_integer(Completions)};

	nomatch ->
	    nomatch
    end.

match(String, RegExp) ->
    re:run(String, RegExp, [{capture, all_but_first, list}]).

is_match(String, RegExp) ->
    case re:run(String, RegExp) of
	nomatch ->
	    false;
	_Else ->
	    true
    end.

to_number([]) ->
    undefined;
to_number(Num) ->
    StrNum = lists:append(string:tokens(Num, " ")),
    try 
	list_to_float(StrNum)
    catch
	_:_ ->
	    list_to_integer(StrNum)
    end.

to_integer([]) ->
    undefined;
to_integer(Num) ->
    list_to_integer(lists:append(
		      string:tokens(Num, " "))).



convert_date_e_s({Year, Month, Day}) 
  when Month < 10, Day < 10 ->
    integer_to_list(Year) ++ "-0" ++ integer_to_list(Month) ++ "-0" ++
	integer_to_list(Day);
convert_date_e_s({Year, Month, Day}) 
  when Month < 10, Day >= 10 ->
    integer_to_list(Year) ++ "-0" ++ integer_to_list(Month) ++ "-" ++
	integer_to_list(Day);
convert_date_e_s({Year, Month, Day}) 
  when Month >= 10, Day < 10 ->
    integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-0" ++
	integer_to_list(Day);
convert_date_e_s({Year, Month, Day}) ->
    integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++
	integer_to_list(Day).

convert_time_e_s({Hour, Minute, Second}) 
  when Hour < 10, Minute < 10, Second < 10 ->
    "0" ++ integer_to_list(Hour) ++ ":0" ++ integer_to_list(Minute) ++ ":0" ++
	integer_to_list(Second);
convert_time_e_s({Hour, Minute, Second}) 
  when Hour < 10, Minute < 10, Second >= 10 ->
    "0" ++ integer_to_list(Hour) ++ ":0" ++ integer_to_list(Minute) ++ ":" ++
	integer_to_list(Second);
convert_time_e_s({Hour, Minute, Second}) 
  when Hour < 10, Minute >= 10, Second < 10 ->
    "0" ++integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":0" ++
	integer_to_list(Second);
convert_time_e_s({Hour, Minute, Second}) 
  when Hour < 10, Minute >= 10, Second >= 10 ->
    "0" ++integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++
	integer_to_list(Second);
convert_time_e_s({Hour, Minute, Second}) ->
    integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++
	integer_to_list(Second).


convert_date_s_e(DateString) ->
    Year = list_to_integer(lists:sublist(DateString, 1, 4)),
    Month = list_to_integer(lists:sublist(DateString, 6, 2)),
    Day = list_to_integer(lists:sublist(DateString, 9, 2)),
    {Year, Month, Day}.
