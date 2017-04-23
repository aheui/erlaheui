#!/bin/bash
erlc erlaheui.erl
if [ -e snippets ]; then
    cd snippets
    git pull
else
    git clone http://github.com/aheui/snippets
    cd snippets
fi
chmod 755 ../run.sh

if [ $# -eq 0 ]; then
    AHEUI="../run.sh" bash ./test.sh standard
else
    AHEUI="../run.sh" bash ./test.sh $1
fi
