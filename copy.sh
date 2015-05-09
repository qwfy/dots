#!/bin/bash

REPO=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

to_repo() {
    cp ~/.vimrc $REPO/vimrc
    cp ~/bin/pac.js $REPO/pac.js
}

to_os() {
    cp $REPO/.vimrc ~/.vimrc
    cp $REPO/pac.js ~/bin/pac.js
}

$1
