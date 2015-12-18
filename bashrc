shopt -s histappend
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups

export PROMPT_COMMAND="history -a; history -c; history -r;"
export PS1='\[\e[32m\][\u@\h \w]\[\e[0m\]\n\[\e[32m\]<<\[\e[0m\] '

export PATH=$PATH:~/bin:~/.cabal/bin


# aliases
alias fuckccp='echo "CCP is being fucked ...";ssh -N -D 9050 root@aixon.co'
alias hs='history | grep -i'
alias l='ls'
alias la='ls -a'
alias ll='ls -l'
alias ls='ls --color=auto'
alias vi=vim
alias vir='vim -R'
alias x='exit'

function update-vim-plugins {
    orig_dir=$(pwd)

    cd ~/.vim/bundle/
    for d in *; do
        cd "$d"
        echo "Updating $d"
        git pull
        cd -
    done

    cd "$orig_dir"
}
