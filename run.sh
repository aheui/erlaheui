#!/bin/bash
erl -noshell -pa pwd -eval "erlaheui:c('$1'), init:stop()"
