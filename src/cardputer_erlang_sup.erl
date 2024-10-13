% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%%%-------------------------------------------------------------------
%% @doc cardputer_erlang top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cardputer_erlang_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = {one_for_one, 1, 1},
    ChildSpecs = [
        {
            cardputer_erlang_shell,
            {cardputer_erlang_shell, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [cardputer_erlang_shell]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
