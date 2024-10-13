% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(cardputer_erlang_shell).

-include("cardputer_keyboard.hrl").

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    start_line :: {non_neg_integer(), non_neg_integer()},
    previous_pressed = undefined,
    eval_str = []
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    m5:begin_([{clear_display, true}]),
    m5_display:clear(),
    m5_display:set_epd_mode(fastest),
    m5_display:set_brightness(128),
    m5_display:set_text_size(1),
    m5_display:start_write(),
    m5_display:set_cursor(0, 0),
    Version = erlang:system_info(atomvm_version),
    m5_display:println(<<"AtomVM ", Version/binary>>),
    m5_display:print(<<">">>),
    m5_display:end_write(),
    cardputer_keyboard:init(),
    {ok, #state{start_line = m5_display:get_cursor()}, 100}.

handle_call(_Message, _From, State) ->
    {noreply, State, 100}.

handle_cast(_Message, State) ->
    {noreply, State, 100}.

handle_info(timeout, State) ->
    NewState = case cardputer_keyboard:keys_pressed() of
        [] -> State#state{previous_pressed = undefined};
        List ->
            KeyEvent = cardputer_keyboard:keycodes_to_event(List),
            case State#state.previous_pressed =:= KeyEvent of
                true -> State;
                false ->
                    process_event(KeyEvent, State#state{previous_pressed = KeyEvent})
            end
    end,
    {noreply, NewState, 100};
handle_info(_Other, State) ->
    io:format("Unexpected message ~p\n", [_Other]),
    {noreply, State, 100}.

terminate(_Reason, State) ->
    {ok, State}.

process_event(#key_pressed{code = ?KEY_CODE_OK}, #state{eval_str = Script} = State) ->
    {ok, Tokens, _} = erl_scan:string(lists:reverse(Script)),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, []),
    m5_display:start_write(),
    m5_display:print(iolist_to_binary(io_lib:format("~n~p~n>", [Result]))),
    m5_display:end_write(),
    State#state{start_line = m5_display:get_cursor(), eval_str = []};
process_event(#key_pressed{code = ?KEY_CODE_BACKSPACE}, #state{eval_str = []} = State) ->
    State;
process_event(#key_pressed{code = ?KEY_CODE_BACKSPACE}, #state{start_line = {_, StartY}, eval_str = [_Last | Tail]} = State) ->
    {_, CurrentY} = m5_display:get_cursor(),
    m5_display:start_write(),
    SavedColor = m5_display:get_raw_color(),
    m5_display:set_color(0, 0, 0),
    m5_display:write_fill_rect(0, StartY, m5_display:width(), CurrentY - StartY + m5_display:font_height()),
    ok = m5_display:set_raw_color(SavedColor),
    m5_display:set_cursor(0, StartY),
    m5_display:print(<<">">>),
    m5_display:print(list_to_binary(lists:reverse(Tail))),
    State#state{eval_str = Tail};
process_event(#key_pressed{ascii = undefined}, State) ->
    State;
process_event(#key_pressed{ascii = Ascii}, State) ->
    m5_display:start_write(),
    m5_display:print(iolist_to_binary([Ascii])),
    m5_display:end_write(),
    State#state{eval_str = [Ascii | State#state.eval_str]}.
