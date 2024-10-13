% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(cardputer_keyboard).

-export([
    init/0,
    keys_pressed/0,
    keycodes_to_event/1
]).

-export_type([
    keycode/0
]).

-include("cardputer_keyboard.hrl").

-type keycode() :: 0..55.

-define(GPIO_OUTPUTS, [13, 15, 3, 4, 5, 6, 7]).
-define(GPIO_ADDR_SELECT, [11, 9, 8]).  % reversed

%% @doc Init cardputer keyboard, configuring GPIOs
init() ->
    _GPIO = gpio:start(),
    lists:foreach(fun(Pin) ->
        ok = gpio:set_pin_mode(Pin, input),
        ok = gpio:set_pin_pull(Pin, up)
    end, ?GPIO_OUTPUTS),
    lists:foreach(fun(Pin) ->
        ok = gpio:set_pin_mode(Pin, output),
        ok = gpio:digital_write(Pin, 0)
    end, ?GPIO_ADDR_SELECT).

%% @doc Return the list of key codes for keys that are pressed
-spec keys_pressed() -> [keycode()].
keys_pressed() ->
    keys_pressed_0(0, []).

keys_pressed_0(8, Acc) -> Acc;
keys_pressed_0(N, Acc) ->
    write_output(N, ?GPIO_ADDR_SELECT),
    case read_input(?GPIO_OUTPUTS, 0, []) of
        [] -> keys_pressed_0(N + 1, Acc);
        Vals ->
            Keys = [N * 7 + Key || Key <- Vals],
            keys_pressed_0(N + 1, Keys ++ Acc)
    end.

write_output(0, []) -> ok;
write_output(X, [Pin | Tail]) ->
    ok = gpio:digital_write(Pin, X band 1),
    write_output(X bsr 1, Tail).

read_input([], _X, Acc) -> Acc;
read_input([Pin | Tail], X, Acc) ->
    NewAcc = case gpio:digital_read(Pin) of
        high -> Acc;
        low -> [X | Acc]
    end,
    read_input(Tail, X + 1, NewAcc).

%% @doc Convert a list of keycodes to a key_pressed record event.
-spec keycodes_to_event([keycode()]) -> #key_pressed{}.
keycodes_to_event(Codes) ->
    Event0 = #key_pressed{
        code = 0,
        codes = Codes,
        control = false,
        option = false,
        alt = false,
        fn = false,
        shift = false,
        ascii = undefined
    },
    {Event1, OtherKeys} = process_modifiers(Codes, Event0, []),
    case {Codes, OtherKeys} of
        {[SingleCode], []} -> Event1#key_pressed{code = SingleCode};
        {_, [SingleCode]} ->
            case Event1#key_pressed.shift of
                true -> Event1#key_pressed{code = SingleCode, ascii = binary:at(?CODE_TO_ASCII_SHIFT, SingleCode)};
                false -> Event1#key_pressed{code = SingleCode, ascii = binary:at(?CODE_TO_ASCII, SingleCode)}
            end;
        _ -> Event1
    end.    

process_modifiers([], Event, Others) -> {Event, Others};
process_modifiers([?KEY_CODE_CTRL | Tail], AccEvent, AccOthers) ->
    process_modifiers(Tail, AccEvent#key_pressed{control = true}, AccOthers);
process_modifiers([?KEY_CODE_OPT | Tail], AccEvent, AccOthers) ->
    process_modifiers(Tail, AccEvent#key_pressed{option = true}, AccOthers);
process_modifiers([?KEY_CODE_ALT | Tail], AccEvent, AccOthers) ->
    process_modifiers(Tail, AccEvent#key_pressed{alt = true}, AccOthers);
process_modifiers([?KEY_CODE_FN | Tail], AccEvent, AccOthers) ->
    process_modifiers(Tail, AccEvent#key_pressed{fn = true}, AccOthers);
process_modifiers([?KEY_CODE_SHIFT | Tail], AccEvent, AccOthers) ->
    process_modifiers(Tail, AccEvent#key_pressed{shift = true}, AccOthers);
process_modifiers([Other | Tail], AccEvent, AccOthers) ->
    process_modifiers(Tail, AccEvent, [Other | AccOthers]).
