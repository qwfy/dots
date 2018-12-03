# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s histappend
export HISTCONTROL=ignoredups:erasedups

# prompts releated
export PROMPT_COMMAND="history -a; history -c; history -r"

# git status in prompt
source /usr/share/git/completion/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1     # + for staged, * if unstaged.
export GIT_PS1_SHOWSTASHSTATE=1     # $ if something is stashed.
export GIT_PS1_SHOWUNTRACKEDFILES=1 # if there are untracked files.
export GIT_PS1_SHOWUPSTREAM='auto'  # <,>,<> behind, ahead, or diverged from upstream.
export PS1='\[\e[32m\][\u@\h \w$(__git_ps1 " (%s)")]\[\e[0m\]\n\[\e[32m\]<<\[\e[0m\] '
# export PS1='\[\e[32m\][\u@\h \w]\[\e[0m\]\n\[\e[32m\]<<\[\e[0m\] '

source /usr/share/fzf/key-bindings.bash

function fuckccp {
    systemctl --user start fuckccp.service
}

function refuck_ccp_and_that_bitchs_father_mother_brother_and_any_other_sucker {
    systemctl --user restart fuckccp.service
}

function anyconnect {
    # sudo openconnect -c ~/bin/http.p12.pem a03.any00.com
    sudo openconnect -c ~/bin/http.p12.pem a01.any00.com
    # sudo openconnect -c ~/bin/fuckccp.p12 a01.any00.com --servercert sha256:864fc57ec1699f89708bbdb59d7312606b42f4420aca2743b4d24378cf7c76ed
    # sudo openconnect -c ~/bin/1840.cert a01.any00.com
}

function aws-get-instance {
    instanceType=$1
    instanceId=$(aws ec2 describe-instances --filters "Name=instance-state-name,Values=stopped,Name=instance-type,Values=$instanceType" --query "Reservations[0].Instances[0].InstanceId")
    instanceId=$(echo $instanceId | tr -d '"')
    echo $instanceId
    export instanceId=$instanceId
}

function aws-get-p2 {
    aws-get-instance "p2.xlarge"
}

function aws-get-t2 {
    aws-get-instance "t2.xlarge"
}

function aws-ip {
    instanceIp=$(aws ec2 describe-instances --filters "Name=instance-id,Values=$instanceId" --query "Reservations[0].Instances[0].PublicIpAddress")
    instanceIp=$(echo $instanceIp | tr -d '"')
    echo $instanceIp
    export instanceIp=$instanceIp
}

function aws-start {
    aws ec2 start-instances --instance-ids $instanceId || exit 1
    aws ec2 wait instance-running --instance-ids $instanceId || exit 1
    aws-ip
}

# aliases

alias aws-ssh='mosh ubuntu@$instanceIp'
alias aws-stop='aws ec2 stop-instances --instance-ids $instanceId'
alias aws-state='aws ec2 describe-instances --instance-ids $instanceId --query "Reservations[0].Instances[0].State.Name"'
alias aws-nb='xdg-open https://$instanceIp:8888'

# alias rm='printf "======= command disabled, use trash instead =======\n"'
alias v='nvim-qt'
alias vi='nvim'
alias vim='nvim'
alias :e='nvim'
alias vir='vim -R'
alias hs='history | grep -i'
alias l='ls -F'
alias bk='cd ../'
alias bkk='cd ../../'
alias bkkk='cd ../../../'
alias bkkkk='cd ../../../../'
alias bkkkkk='cd ../../../../../'
alias x='exit'
alias sg='stack ghci'
alias purgepkg='sudo pacman -R --cascade --nosave --recursive'
alias aria=aria2c
alias open='xdg-open'
alias jc='jupyter console'
alias jn2='jupyter notebook --port 2222 --ip=riemann'
alias jn3='jupyter notebook --port 3333 --ip=riemann'
alias dlrussell='aria2c --http-user=incomplete --http-passwd=XzwDGZOULZ8yWmaK0PB8GF8NKC4xtJA7'
alias catkin_make="PATH=/opt/ros/kinetic/bin:$PATH catkin_make -DPYTHON_EXECUTABLE=/usr/bin/python2 -DPYTHON_INCLUDE_DIR=/usr/include/python2.7 -DPYTHON_LIBRARY=/usr/lib/libpython2.7.so"
alias cdtemp='cd ~/temp'
[ -f /opt/miniconda3/etc/profile.d/conda.sh ] && source /opt/miniconda3/etc/profile.d/conda.sh
