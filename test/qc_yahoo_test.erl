-module(qc_yahoo_test).

-include_lib("eunit/include/eunit.hrl").
-include("quote_crawl.hrl").

request_test() ->
    meck:new(httpc),
    meck:expect(httpc, request, 
		fun(_Url) -> 
			{ok, {bla, lsda,
			      "433.10\r\n90.80\r\n"}} 
		end),
    "433.10\r\n90.80\r\n" = qc_yahoo:request("mtg-b.st+eric-b.st", "a"),
    true = meck:validate(httpc),
    meck:unload(httpc).
    

get_test() ->
    meck:new(httpc),
    meck:expect(httpc, request, 
		fun(_Url) -> 
			{ok, {bla, lsda,
			      "90.80\r\n"}} 
		end),
    [{'eric-b.st', [{ask, 90.8}]}] = qc_yahoo:get(['eric-b.st'], [ask]),
    true = meck:validate(httpc),
    meck:unload(httpc),
    %% Test with ask and bid price 
    meck:new(httpc),
    meck:expect(httpc, request, 
		fun(_Url) -> 
			{ok, {bla, lsda,
			      "90.80,10.4\r\n"}} 
		end),
    [{'eric-b.st', [{ask, 90.8},{"b", "10.4"}]}] = 
	qc_yahoo:get(['eric-b.st'], [ask, "b"]),
    true = meck:validate(httpc),
    meck:unload(httpc),
    %% Tets with several quotes
    meck:new(httpc),
    meck:expect(httpc, request, 
		fun(_Url) -> 
			{ok, {bla, lsda,
			      "90.80,10.4\r\n10,11.3\r\n"}} 
		end),
    [{'eric-b.st', [{ask, 90.8},{"b", "10.4"}]},
     {'abb.st', [{ask, 10}, {"b", "11.3"}]}] = 
	qc_yahoo:get(['eric-b.st', 'abb.st'], [ask, "b"]),
    true = meck:validate(httpc),
    meck:unload(httpc),
    %% Test with all allowed atom values
    meck:new(httpc),
    meck:expect(httpc, request, 
		fun(_Url) -> 
			{ok, {bla, lsda,
			      "90.80,10.4,10,11.3\r\n"}} 
		end),
    [{'eric-b.st', [{ask, 90.8},{bid, 10.4}, {high, 10}, {low, 11.3}]}] = 
	qc_yahoo:get(['eric-b.st'], [ask, bid, high, low]),
    true = meck:validate(httpc),
    meck:unload(httpc).

    
