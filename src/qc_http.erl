%%% File    : qc_http.erl
%%% Author  : Erik Axling <dude@CodeMachine>
%%% Created :  1 May 2011 by Erik Axling <dude@CodeMachine>

-module(qc_http).

-export([get_text_tv/0, get_text_tv/1]).

get_text_tv() ->
    get_text_tv(203).

get_text_tv(Page) when is_integer(Page) ->
    Url = "http://svt.se/svttext/web/pages/" ++ integer_to_list(Page)  ++ 
	".html",
    {ok, {{_, 200, _}, _, Body}} = httpc:request(Url),
    Body.
