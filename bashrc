PS1='[\u@\h \w]\n<< '

export HISTCONTROL=ignoredups:erasedups
shopt -s histappend
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

# aliases
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -a'
alias vi=vim
alias vir='vim -R'
alias x='exit'
alias hg='history |grep -i'
