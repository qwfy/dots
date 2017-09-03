# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export HISTCONTROL=ignoredups:erasedups
shopt -s histappend
export PROMPT_COMMAND="history -a; history -c; history -r"
export EDITOR=nvim
#export OPAMUSEINTERNALSOLVER="true"

export PATH=~/bin:~/bin/nodejs/bin:~/.local/bin:~/.go/bin:$PATH
export GOPATH=~/.go/

source /usr/share/git/completion/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1     # + for staged, * if unstaged.
export GIT_PS1_SHOWSTASHSTATE=1     # $ if something is stashed.
export GIT_PS1_SHOWUNTRACKEDFILES=1 # if there are untracked files.
export GIT_PS1_SHOWUPSTREAM='auto'  # <,>,<> behind, ahead, or diverged from upstream.

export PS1='\[\e[32m\][\u@\h \w$(__git_ps1 " (%s)")]\[\e[0m\]\n\[\e[32m\]<<\[\e[0m\] '

# aliases
alias vi='nvim'
alias vim='nvim'
alias hs='history | grep -i'
alias l='ls'
alias la='ls -a'
alias ll='ls -lh'
alias ls='ls --color=auto'
alias vir='vim -R'
alias x='exit'
alias sg='stack ghci'
alias purgepkg='sudo pacman -R --cascade --nosave --recursive'
alias bk='cd ../'
alias bkk='cd ../../'
alias bkkk='cd ../../../'
alias bkkkk='cd ../../../../'
alias bkkkkk='cd ../../../../../'


function fuckccp {
    systemctl --user start fuckccp.service
}

function refuck_ccp_and_that_bitchs_father_mother_brother_and_any_other_sucker {
    systemctl --user restart fuckccp.service
}

function anyconnect {
    sudo openconnect -c ~/bin/fuckccp.p12 a03.blockcn.net
}

source /usr/share/fzf/key-bindings.bash
