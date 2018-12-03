[[ -f ~/.bashrc ]] && . ~/.bashrc
[[ -f ~/.profile ]] && . ~/.profile

# OPAM configuration
source /home/incomplete/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
if [ -e /home/incomplete/.nix-profile/etc/profile.d/nix.sh ]; then . /home/incomplete/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
