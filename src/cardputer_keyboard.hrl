% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

% Keycodes don't match M5Cardputer repository because I couldn't figure out
% exactly how they computed it.
% Each key has an X, Y coordinate, X being the address send to the 74HC138 (0-7)
% and Y being the GPIO bit that is set to 0.
% Here we are encoding X with 8, 9, 11 in this order (LSB: 8) and
% we are processing Y in 13, 15, 3, 4, 5, 6, 7 order (13 = 0, 15 = 1, 3 = 2, 4 = 3, etc.)
% Codes are defined as X * 7 + Y

-define(KEY_CODE_OPT, 0).
-define(KEY_CODE_CTRL, 7).
-define(KEY_CODE_ALT, 8).
-define(KEY_CODE_SHIFT, 28).
-define(KEY_CODE_FN, 35).
-define(KEY_CODE_OK, 34).
-define(KEY_CODE_BACKSPACE, 48).
-define(KEY_CODE_ESC, 49).

-define(CODE_TO_ASCII, <<
    0, % opt
    $z,
    $c,
    $b,
    $m,
    $.,
    $ ,
    0, % ctrl
    0, % alt
    $x,
    $v,
    $n,
    $,,
    $/,
    $q,
    $e,
    $t,
    $u,
    $o,
    $[,
    $\\,
    $\t,
    $w,
    $r,
    $y,
    $i,
    $p,
    $],
    0, % SHIFT
    $s,
    $f,
    $h,
    $k,
    $;,
    $\n,
    0, % fn
    $a,
    $d,
    $g,
    $j,
    $l,
    $',
    $1,
    $3,
    $5,
    $7,
    $9,
    $_,
    $\b,
    $`,
    $2,
    $4,
    $6,
    $8,
    $0,
    $=
>>).

-define(CODE_TO_ASCII_SHIFT, <<
    0, % opt
    $Z,
    $C,
    $B,
    $M,
    $>,
    $ ,
    0, % ctrl
    0, % alt
    $X,
    $V,
    $N,
    $<,
    $?,
    $Q,
    $E,
    $T,
    $U,
    $O,
    ${,
    $|,
    $\t,
    $W,
    $R,
    $Y,
    $I,
    $P,
    $},
    0, % SHIFT
    $S,
    $F,
    $H,
    $K,
    $:,
    $\n,
    0, % fn
    $A,
    $D,
    $G,
    $J,
    $L,
    $",
    $!,
    $#,
    $\%,
    $&,
    $(,
    $-,
    $\b,
    $~,
    $@,
    $$,
    $^,
    $*,
    $),
    $+
>>).

-record(key_pressed, {
    code :: non_neg_integer(),  % code for main key
    codes :: [non_neg_integer()], % code for all keys
    control :: boolean(),
    option :: boolean(),
    alt :: boolean(),
    fn :: boolean(),
    shift :: boolean(),
    ascii :: non_neg_integer() | undefined
}).
