#!/bin/bash

if [ -e snippets ]; then
    cd snippets
    git pull
else
    git clone http://github.com/aheui/snippets
fi
cd snippets
chmod 755 ../run.sh
AHEUI="../run.sh" bash test.sh standard
