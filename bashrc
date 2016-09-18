export HISTCONTROL=ignoredups:erasedups
shopt -s histappend
export PROMPT_COMMAND="history -a; history -c; history -r"
export EDITOR=vim

export PATH=~/.local/bin/:~/bin/:$PATH

source /usr/share/git/completion/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1     # + for staged, * if unstaged.
export GIT_PS1_SHOWSTASHSTATE=1     # $ if something is stashed.
export GIT_PS1_SHOWUNTRACKEDFILES=1 # if there are untracked files.
export GIT_PS1_SHOWUPSTREAM='auto'  # <,>,<> behind, ahead, or diverged from upstream.

export PS1='\[\e[32m\][\u@\h \w$(__git_ps1 " (%s)")]\[\e[0m\]\n\[\e[32m\]<<\[\e[0m\] '

# aliases
alias hs='history | grep -i'
alias l='ls'
alias la='ls -a'
alias ll='ls -l'
alias ls='ls --color=auto'
alias vi=vim
alias vir='vim -R'
alias x='exit'
alias sg='stack ghci'
alias bk='cd ../'
alias bkk='cd ../../'
alias bkkk='cd ../../../'
alias bkkkk='cd ../../../../'


function fuckccp {
    notify-send "Proxy started";
    ssh -N -D 9050 root@proxy;
    notify-send "Proxy closed";
    fuckccp
}

function update-vim-plugins {
    orig_dir=$(pwd)

    cd ~/.vim/bundle/
    for d in *; do
        cd "$d"
        echo "Updating $d"
        git pull
        cd -
    done
    cd ~/.vim/bundle/vimproc.vim/ && make

    cd "$orig_dir"
}
