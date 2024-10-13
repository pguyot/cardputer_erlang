cardputer_erlang
=====

Small M5 Cardputer demo using AtomVM.

Build and flash
---------------

You need to copy some stdlib modules:

    $ mkdir -p priv/stdlib-6.0/ebin
    $ cp /opt/local/lib/erlang/lib/stdlib-6.0/ebin/{erl_anno.beam,erl_eval.beam,erl_internal.beam,erl_lint.beam,erl_parse.beam,erl_scan.beam,gb_sets.beam,orddict.beam,ordsets.beam,otp_internal.beam,string.beam,unicode_util.beam} priv/stdlib-6.0/ebin/

Then the application can be compiled and flashed.

    $ rebar3 atomvm esp32_flash -p /dev/cu.usbmodem*

