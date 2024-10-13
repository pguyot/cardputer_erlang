% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(cardputer_erlang).

-export([start/0]).

%% @doc Entry point of Cardputer demo.
start() ->
    {ok, _Supervisor} = cardputer_erlang_sup:start_link(),
    receive stop -> ok end,
    ok.
