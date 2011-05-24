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
    meck:unload(httpc).
    
