%-----------------------------------------------------------------------------------------------
% @doc newsman server
% 
% Every 5 minutes fetches top stories indexes from hacker-news.
% On receiving stories indexes send fist 50 indexes to newsroom application.
% 
% newsroom in its turn sends request to fetch stories from hacker-news.
% newsman fetches required stories and send them to newsroom.
% 
% All communications between newsman and newsroom are asynchronous.
% 
% @author Anatoly Rodionov <anatoly.ya.rodionov@gmail.com>
% @end
%-----------------------------------------------------------------------------------------------

-module(newsman).
-behavior(gen_server).
-vsn(1.0).

%-----------------------------------------------------------------------------------------------
-include("log.hrl"). 

% In this implementation this makes little  sense, but it is show time.
-compile({inline, [topstories/1, decode/1]}).

%-----------------------------------------------------------------------------------------------

-define(HACKERS,    "https://hacker-news.firebaseio.com/v0/").
-define(TOPSTORIES, ?HACKERS"topstories.json").
-define(ITEM,       ?HACKERS"item/").

%-----------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).


-export([grab/2, item/1]).

%-----------------------------------------------------------------------------------------------
-record(s_t, {
    len    ::  pos_integer(),     % how many top stories to fetch
    period ::  pos_integer()      % keep this information for debugging only
    }).
% -type s_t() :: #s_t{}.

%-----------------------------------------------------------------------------------------------
% Interfaces
%-----------------------------------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
% @doc Starts {@module}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

%-----------------------------------------------------------------------------------------------
-spec grab([pos_integer()], [pos_integer()]) -> any().
% @doc Grabs new stories and sends them back to newsroom.
% Intentionally asynchronous.

grab(NewIndexes, Indexes) ->  
    gen_server:cast(?MODULE, {grab, NewIndexes, Indexes}).

%-----------------------------------------------------------------------------------------------
% @doc Initializes {@module}.

init({})  ->
    process_flag(trap_exit, true),

    {ok, Len}    = application:get_env(hna, newsman_len),
    {ok, Period} = application:get_env(hna, newsman_period),
    ST = #s_t{
        len    = Len,
        period = Period
    },
    {ok, _} = timer:send_interval(Period, topstories),
    self() ! topstories,
    ?DEBUG("~p ST=~p~n", [?M_F, ?PRECORD(ST)]),
    {ok, ST}.

%-----------------------------------------------------------------------------------------------
% Callbacks
%-----------------------------------------------------------------------------------------------

handle_cast(_Msg={grab, NewIndexes, Indexes}, ST) ->
    ?DEBUG("~p: _Msg:~p~n", [?M_F_L, _Msg]),
    newsroom:merge(items(NewIndexes), Indexes),
    {noreply, ST};

handle_cast(_Msg, ST) ->
    ?ERROR("~p: unknown _Msg:~p~n", [?M_F_L, _Msg]),
    {noreply, ST}.

handle_call(_Msg, _From, ST) ->
    ?ERROR("~p: unknown _Msg:~p~n", [?M_F_L, _Msg]),
    {reply, false, ST}.

handle_info(topstories, ST) ->
    self() ! {topstories, ST#s_t.len},
    {noreply, ST};

handle_info({topstories, Len}, ST) ->
    case topstories(Len) of
        {ok, Indexes} -> newsroom:advise(Indexes);
        _             -> ok
    end,
    {noreply, ST};

handle_info(_Msg, ST) ->
    ?ERROR("~p: unknown _Msg:~p~n", [?M_F_L, _Msg]),
    {noreply, ST}.

code_change(_OldVsn, ST, _Extra) ->
    {ok, ST}.

% @doc Stops {@module}.
terminate(_Msg, _ST) ->
    ?INFO("~p:terminate: ~p~n", [?M_F_L, _Msg]),
    terminated.

%-----------------------------------------------------------------------------------------------
% private
%-----------------------------------------------------------------------------------------------
-spec request(string()) -> {ok, binary()} | {error, term()}.
% @doc helper to call httpc:request get with binary body return
% @param URL url for request
% @returns {ok, body} or {error, reason}

request(URL) -> convert(httpc:request(get, {URL, []}, [], [{body_format, binary}])).

%-----------------------------------------------------------------------------------------------
-spec convert({ok, term()} | {error, term()}) -> {ok, binary()} | {error, term()}.
% @doc Converts http result to more convenient form.

convert({ok, {{_, 200, _}, _Headers, Body}})        -> {ok, Body};
convert({ok, {{_, Code, Reason}, _Headers, _Body}}) -> {error, {Code, Reason}};
convert({ok, _})                                    -> {error, not_supported};
convert(R = {error, _Reason})                       -> R.

%---------------------------------binary_to_integer--------------------------------------------------------------
-spec topstories(pos_integer()) -> {ok, [pos_integer()]} | {error, term()}.
% @doc Calls hackers-news API topstories; selects first L (50) indexes
% @param L number of indexes requested
% @returns {ok, [index]} or {error, reason}

topstories(L) -> topstories(L, request(?TOPSTORIES)).

-spec topstories(pos_integer(), tuple()) -> {ok, [pos_integer()]} | {error, term()}.
% @doc Call hackers-news API topstories; select first L indexes
% @param L number of indexes requested
% @param H result of http call
% @returns {ok, [index]} or {error, reason}

topstories(L, {ok, Body}) -> {ok, lists:sublist(decode(Body), L)};
topstories(_L, R)         -> R.

%------------------------------------------------------------------- ----------------------------
-spec item
    (pos_integer())   -> {pos_integer(), binary() | false};
    ({ok, binary()})  -> binary() | false;
    ({error, term()}) -> false. 
% @doc Returns item or false if item not found or error occurs
% @param Index of item 

item(Index) when is_integer(Index) ->  {Index, item(request(?ITEM ++ integer_to_list(Index) ++ ".json"))};
item({ok, <<"null">>})             ->  false;                  % not found
item({ok, Body})                   ->  Body;
item(_)                            ->  false.

%------------------------------------------------------------------- ----------------------------
-spec items([pos_integer()]) -> [{pos_integer(), binary()}].
% @doc Returns list of pairs of found items as {index, story}

% items(Indexes) -> [{I, M} || {I, M} <- lists:map(fun item/1, Indexes), M /= false].
items(Indexes) -> [{I, M} || {I, M} <- rpc:pmap({?MODULE, item}, [], Indexes), M /= false].
% The last version works 3 times faster on 50 items

%------------------------------------------------------------------- ----------------------------
-type json_value() :: term().
-spec decode(binary()) -> json_value().
% @doc Decodes json format. Later, if we need 
% more compact implementation we can write special function for this particular case,
% @param Body binary with json format data

decode(Body) -> jsone:decode(Body).

%------------------------------------------------------------------ ----------------------------
