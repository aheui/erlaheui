#!/bin/bash
erl -noshell -pa ../ -eval "erlaheui:c('$1'), init:stop()"
