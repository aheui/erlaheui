#!/usr/bin/env escript
%% -*- erlang -*-

main([Path]) ->
    compile:file("../../src/erlaheui.erl"),
    erlaheui:c(Path);
main(_) ->
    usage().

usage() ->
    io:format("usage: run aheui script\n"),
    halt(1).

