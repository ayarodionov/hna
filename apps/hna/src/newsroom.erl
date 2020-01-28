%-----------------------------------------------------------------------------------------------
% @doc newsroom server
% 
% newsroom receives from newsman list of first 50 topstories indexes from hackers-news,
% select indexes which stories are not already stored and sends list of new indexes
% to newsman
% 
% newsman fetches required stories and send them to newsroom.
% 
% newsroom merges new stories to stories property list and reordered them in the order
% they are stored in hackers-news.
% 
% All communications between newsman and newsroom are asynchronous.
% 
% @author Anatoly Rodionov <anatoly.ya.rodionov@gmail.com>
% @end
%-----------------------------------------------------------------------------------------------

-module(newsroom).
-behavior(gen_server).
-vsn(1.0).

%-----------------------------------------------------------------------------------------------
-include("log.hrl").

%-----------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-export([advise/1, merge/2]).
-export([indexes/0, story/1, stories/2, item/1, items/2, pos/1]).

% In this implementation this makes little  sense, but it is show time.
-compile({inline, [separate/2, new_ones/2]}).

-ifdef(TEST).
-export([start/0, separate/2, pick/2, new_ones/2, find_pos/2]).
-endif.

%-----------------------------------------------------------------------------------------------
-record(s_t, {
    stories = []     :: [{pos_integer(), binary()}]   % storage
    }).
% -type s_t() :: #s_t{}.

%-----------------------------------------------------------------------------------------------
% Interfaces
%-----------------------------------------------------------------------------------------------
-ifdef(TEST).
-spec start() -> {ok, pid()}.
% @doc Starts {@module}.

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, {}, []).
-endif.

-spec start_link() -> {ok, pid()}.
% @doc Starts {@module}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

%-----------------------------------------------------------------------------------------------
-spec advise([pos_integer()]) -> any().
% @doc Advises new items
% @param Indexes possibly new indexes to add to stories

advise(Indexes) ->  
    gen_server:cast(?MODULE, {advise, Indexes}).

%-----------------------------------------------------------------------------------------------
-spec pos(pos_integer()) -> pos_integer() | -1.
% @doc finds position of the story index 
% @param Index index of story
% @returns position as positive number or -1 if not found

pos(Index) ->
    gen_server:call(?MODULE, {pos, Index}).

%-----------------------------------------------------------------------------------------------
-spec story(pos_integer()) -> binary() | undefined.
% @doc Request to return story 
% @param Index index of story
% @returns story in binary json format or undefined

story(Index) ->
    gen_server:call(?MODULE, {item, Index}).

%-----------------------------------------------------------------------------------------------
-spec stories(pos_integer(), pos_integer()) -> [binary()].
% @doc Request to return stories  
% @param Index index of story
% @param N number of stories 
% @returns list of stories in binary json format or empty list

stories(Index, N) -> items(pos(Index), N).

%-----------------------------------------------------------------------------------------------
-spec item(pos_integer()) -> binary() | undefined.
% @doc Request to return story
% @param Pos starting position
% @returns story in binary json format or undefined

item(Pos) ->
    gen_server:call(?MODULE, {item, Pos}).

%-----------------------------------------------------------------------------------------------
-spec items(pos_integer(), non_neg_integer()) -> [binary()].
% @doc Request to return list of stories 
% @param Pos starting position
% @param N number of stories 
% @returns list of stories in binary json format or empty list

items(Pos, N) ->
    gen_server:call(?MODULE, {items, Pos, N}).

%-----------------------------------------------------------------------------------------------
-spec indexes() -> [pos_integer()].
% @doc Request to return list of all indexes 
% @returns of all indexes

indexes() ->
    gen_server:call(?MODULE, indexes).

%-----------------------------------------------------------------------------------------------
-spec merge([{pos_integer(), term()}], [pos_integer()]) -> any().
% @doc Merge new stories and restore order
% @param News new stories
% @param Indexes order in which stories will be stored

merge(News, Indexes) ->
    gen_server:cast(?MODULE, {merge, News, Indexes}).

%-----------------------------------------------------------------------------------------------
% @doc Initializes {@module}.

init({})  ->
    process_flag(trap_exit, true),
    ST = #s_t{},
    ?DEBUG("~p ST=~p~n", [?M_F, ?PRECORD(ST)]),
    {ok, ST}.

%-----------------------------------------------------------------------------------------------
% Callbacks
%-----------------------------------------------------------------------------------------------

handle_cast(_Msg={advise, Indexes}, ST) -> % normal flow
    ?DEBUG("~p: _Msg:~p~n", [?M_F_L, _Msg]),
    % Even there is no new stories it is ok to call grab because order of indexes
    % may be changed (grab is calling merge, which will reorder stories). 
    % It is possible to take care about it in newsroom itself
    % but using the same path makes code shorter and looks more elegant to me
    newsman:grab(new_ones(Indexes, ST#s_t.stories), Indexes),  % grab new stories
    {noreply, ST};

handle_cast(_Msg={merge, News, Indexes}, ST) -> % normal flow
    ?DEBUG("~p: _Msg:~p~n", [?M_F_L, _Msg]),
    {noreply, ST#s_t{stories = pick(Indexes, News ++ ST#s_t.stories)}};

handle_cast(_Msg, ST) ->
    ?DEBUG("~p: unknown _Msg:~p~n", [?M_F_L, _Msg]),
    {noreply, ST}.

handle_call(_Msg={pos, Index}, _From, ST) ->
    ?DEBUG("~p:_Msg:~p~n", [?M_F_L, _Msg]),
    {reply, find_pos(Index, ST#s_t.stories), ST};

handle_call(_Msg={story, Index}, _From, ST) ->
    ?DEBUG("~p:_Msg:~p~n", [?M_F_L, _Msg]),
    {reply, proplists:get_value(Index, ST#s_t.stories), ST};

handle_call(_Msg={item, Pos}, _From, ST) when Pos < 1 ->
    {reply, undefined, ST};

handle_call(_Msg={item, Pos}, _From, ST) when Pos > length(ST#s_t.stories) ->
    {reply, undefined, ST};

handle_call(_Msg={item, Pos}, _From, ST) ->
    ?DEBUG("~p:_Msg:~p~n", [?M_F_L, _Msg]),
    {_, S} = lists:nth(Pos, ST#s_t.stories),
    {reply, S, ST};

handle_call(_Msg={items, Pos, _N}, _From, ST)  when Pos < 1 ->
     {reply, [], ST};

handle_call(_Msg={items, Pos, _N}, _From, ST)  when Pos > length(ST#s_t.stories) ->
     {reply, [], ST};

handle_call(_Msg={items, _Pos, N}, _From, ST)  when N < 1 ->
     {reply, [], ST};

handle_call(_Msg={items, Pos, N}, _From, ST) ->
    ?DEBUG("~p:_Msg:~p~n", [?M_F_L, _Msg]),
    {reply, [S || {_, S} <- lists:sublist(ST#s_t.stories, Pos, N)], ST};

handle_call(_Msg=indexes, _From, ST) ->
    ?DEBUG("~p:_Msg:~p~n", [?M_F_L, _Msg]),
    {reply, [I || {I, _} <- ST#s_t.stories], ST};

handle_call(_Msg, _From, ST) ->
    ?ERROR("~p: unknown _Msg:~p~n", [?M_F_L, _Msg]),
    {reply, false, ST}.

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
-spec separate([pos_integer()], [{pos_integer(), term()}]) -> {[pos_integer()], [pos_integer()]}.
% @doc Separates old and new items
% @param Indexes list of indexes
% @param Plist property list of already stored data
% @returns pair of lists of new and already presented items

separate(Indexes, Plist) -> separate(Indexes, Plist, [], []).

-spec separate([pos_integer()], [{pos_integer(), term()}], [pos_integer()], [pos_integer()] ) -> 
    {[pos_integer()], [pos_integer()]}.
% @doc Separates old and new items
% @param Indexes list of indexes
% @param Plist property list of already stored data
% @param New accumulated list of new indexes
% @param Old accumulated list of old indexes
% @returns pair of lists of new and already present items

separate([], _Plist, New, Old)          -> {New, Old};
separate([Ind | Tail], Plist, New, Old) -> 
    case proplists:is_defined(Ind, Plist) of
        true  -> separate(Tail, Plist, New, [Ind | Old]);
        false -> separate(Tail, Plist, [Ind | New], Old)
    end.

%-----------------------------------------------------------------------------------------------
-spec new_ones([pos_integer()], [{pos_integer(), term()}]) -> [pos_integer()].
% @doc Finds new items
% @param Indexes list of indexes
% @param Plist property list of already stored data
% @returns lists of new items

new_ones(Indexes, Plist) ->
    {NewOnes, _} = separate(Indexes, Plist),
    NewOnes.

%-----------------------------------------------------------------------------------------------
-spec pick([pos_integer()], [{pos_integer(), term()}]) -> [{pos_integer(), term()}].
% @doc picks elements from property list

pick([], _Plist) -> [];
pick([Ind | Tail], Plist) ->
    case proplists:get_value(Ind, Plist) of
        undefined -> pick(Tail, Plist);
        Val       -> [{Ind, Val} | pick(Tail, Plist)]
    end.

%-----------------------------------------------------------------------------------------------
-spec find_pos(pos_integer(), [{pos_integer(), term()}]) -> pos_integer() | -1.
% @doc find position of element in property list

find_pos(Index, Plist) -> find_pos(Index, Plist, 1).

-spec find_pos(pos_integer(), [{pos_integer(), term()}], pos_integer()) -> pos_integer() | -1.

find_pos(_Index, [], _Current)             -> -1;
find_pos(Index, [{Index, _} | _], Current) -> Current;
find_pos(Index, [_ | Tail], Current)       -> find_pos(Index, Tail, Current + 1).

%-----------------------------------------------------------------------------------------------
