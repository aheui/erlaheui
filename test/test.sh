#!/bin/bash

if [ -e snippets ]; then
    cd snippets
    git pull
else
    git clone https://github.com/aheui/snippets
    cd snippets
fi

if [ $# -eq 0 ]; then
    AHEUI="../test.erl" bash ./test.sh standard
else
    AHEUI="../test.erl" bash ./test.sh $1
fi

