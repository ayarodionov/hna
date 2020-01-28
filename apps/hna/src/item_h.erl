%-----------------------------------------------------------------------------------------------
% @doc item_h
% 
% cowboy HTTP callback.
% 
% API:
% <ol>
% <li>
% <i>http://localhost:8080/?item=N</i> where <i>N</i> is cardinal number of story
% </li>
% <li>
% <i>http://localhost:8080/?item=N&#38;next=M</i> where  is cardinal number of story,
% <i>M</i> - how many stories after story <i>N</i> show on the page
% </li>
% <li>
% <i>http://localhost:8080/?item=N&#38;next</i> - same as <i>http://localhost:8080/?item=N&#38;next=10</i>
% </li>
% </ol>
% 
% @author Anatoly Rodionov <anatoly.ya.rodionov@gmail.com>
% @end
%-----------------------------------------------------------------------------------------------

-module(item_h).
%------------------------------------------------------------------- ----------------------------
-export([init/2]).

-include("log.hrl"). 

-define(RANGE,         10).
-define(CONTENT_TYPE,  <<"application/json; charset=utf-8">>).

%------------------------------------------------------------------- ----------------------------

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	#{item := Index, next := Next} = cowboy_req:match_qs([{item, [], undefined}, {next, [], undefined}], Req0),
	Req = get(Method, Index, Next, Req0),
	?DEBUG("~p  Index=~p Next=~p~n Req0=~p~n",[?M_F, Index, Next, Req0]),
	{ok, Req, Opts}.

%----------------------------------------------------------------------------------------------

get(<<"GET">>, undefined, undefined, Req) ->
	cowboy_req:reply(400, #{}, 
		<<
		"Missing item parameter. Try:\n"
		"http://localhost:8080/?item=N\n"
		"or\n"
		"http://localhost:8080/?item=N&next=M\n"
		"or\n"
		"http://localhost:8080/?item=N&next"
		>>, 
		Req);

get(<<"GET">>, Index, undefined, Req) ->
	?DEBUG("~p Index=~p~n", [?M_F, Index]),
	Ind = b2i(Index),
	case newsroom:item(Ind) of
		undefined ->
			cowboy_req:reply(
				200, 
				#{<<"content-type">> => ?CONTENT_TYPE}, 
				encode(#{possible_items => newsroom:indexes()}), 
				Req);
		Item ->
			cowboy_req:reply(
				200, 
				#{<<"content-type">> => ?CONTENT_TYPE}, 
				Item, 
				Req)
	end;

get(<<"GET">>, Index, Next, Req) ->
	?DEBUG("~p Index=~p Next=~p~n", [?M_F, Index, Next]),
	Ind = b2i(Index),
	N   = case b2i(Next) of -1 -> ?RANGE; I -> I end,
	case newsroom:items(Ind, N) of
		[] ->
			cowboy_req:reply(
				200, 
				#{<<"content-type">> => ?CONTENT_TYPE}, 
				encode(#{possible_items => newsroom:indexes()}), 
				Req);
		Items ->
			cowboy_req:reply(
				200, 
				#{<<"content-type">> => ?CONTENT_TYPE}, 
				combine(Items), 
				Req)
	end;

get(_, _, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

%------------------------------------------------------------------- ----------------------------
-spec b2i(binary()) -> pos_integer() | -1.
% @doc save way to convert binary to integer
% @param B binary, supposedly in right format
% @returns positive number or -1 when format incorrect

b2i(B)->
	try binary_to_integer(B) of
		I -> I
	catch
		_   -> -1;
		_:_ -> -1
	end.

%------------------------------------------------------------------- ----------------------------
-spec combine([binary()]) -> binary().
% @doc Encodes json format. Later, if we need 
% more compact implementation we can write special function for this particular case,

combine(Stories) -> encode(#{stories => [decode(I) || I <- Stories]}).

%------------------------------------------------------------------- ----------------------------
-type json_value() :: term().
-spec decode(binary()) -> json_value().
% @doc Decodes json format. Later, if we need 
% more compact implementation we can write special function for this particular case,
% @param Body binary with json format data

decode(Body) -> jsone:decode(Body).

%------------------------------------------------------------------- ----------------------------
-spec encode(map()) -> binary().
% @doc Encodes json format. Later, if we need 
% more compact implementation we can write special function for this particular case,

encode(M) -> jsone:encode(M).

%------------------------------------------------------------------- ----------------------------
