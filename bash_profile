#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# OPAM configuration
. /home/incomplete/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
. /usr/bin/virtualenvwrapper.sh
