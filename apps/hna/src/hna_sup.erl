%-----------------------------------------------------------------------------------------------
% @doc hna top level supervisor.
% 
% Starts two workers: newsman and newsroom.
% 
% @author Anatoly Rodionov <anatoly.ya.rodionov@gmail.com>
% @end
%-----------------------------------------------------------------------------------------------

-module(hna_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

% @doc Starts {@module}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% @doc Starts two workers: newsman and newsroom.
init([]) ->
	Newsroom = #{
		id       => newsroom,
		start    => {newsroom, start_link, []},
		restart  => permanent,
		shutdown => brutal_kill,
		type     => worker
		},
	Newsman = #{
		id       => newsman,
		start    => {newsman, start_link, []},
		restart  => permanent,
		shutdown => brutal_kill,
		type     => worker
		},
    SupFlags = #{
    	strategy  => one_for_one,
    	intensity => 1,
        period    => 1
        },
    ChildSpecs = [Newsroom, Newsman], % order important
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
