%-----------------------------------------------------------------------------------------------
% @doc hna public API
% 
% Starts hna application.
% 
% Also starts cowboy.
% 
% @author Anatoly Rodionov <anatoly.ya.rodionov@gmail.com>
% @end
%-----------------------------------------------------------------------------------------------

-module(hna_app).

-behaviour(application).

-export([start/2, stop/1]).

%-----------------------------------------------------------------------------------------------
-define(HTTP_PORT, 8080).
-define(ROOT, "/").

%-----------------------------------------------------------------------------------------------
% @doc Starts {@module} and cowboy. cowboy compiles file item_h.erl.

start(_StartType, _StartArgs) ->

	ItemH = cowboy_router:compile([
		{'_', [
			{?ROOT, item_h, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(
		http, 
		[{port, ?HTTP_PORT}], 
		#{env => #{dispatch => ItemH}}),

    hna_sup:start_link().

% @doc Stops {@module}.
stop(_State) ->
    ok.

%-----------------------------------------------------------------------------------------------
