#!/bin/bash

if [ -e snippets ]; then
    cd snippets
    git pull
else
    git clone http://github.com/aheui/snippets
    cd snippets
fi
chmod 755 ../run.sh
AHEUI="../run.sh" bash ./test.sh standard
