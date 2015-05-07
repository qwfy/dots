#!/bin/bash

REPO=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

to_repo() {
    cp ~/.vimrc $REPO/.vimrc
}

to_os() {
    cp $REPO/.vimrc ~/.vimrc
}

$1
