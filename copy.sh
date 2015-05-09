#!/bin/bash

set -e

REPO=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

to_repo() {
    cp ~/.vimrc $REPO/vimrc
    cp ~/bin/pac.js $REPO/pac.js
    cp ~/.xinitrc $REPO/xinitrc
    cp ~/.xmodmap $REPO/xmodmap
}

to_os() {
    cp $REPO/vimrc ~/.vimrc
    cp $REPO/pac.js ~/bin/pac.js
    cp $REPO/xinitrc ~/.xinitrc
    cp $REPO/xmodmap ~/.xmodmap
}

$1
