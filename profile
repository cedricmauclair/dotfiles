# No core files by default
ulimit -S -c 0 >/dev/null 2>&1


# History configurations
export HISTSIZE=32768
export HISTFILESIZE=32768
export HISTIGNORE=cd:ls
export HISTCONTROL=ignoreboth:erasedups
export PROMPT_COMMAND="history -a; ${PROMPT_COMMAND}"


# Platform/host specific settings
EXTRAS=(
  "$HOME/.profile.$(uname)"
  "$HOME/.profile.$(hostname)")

for EXTRA in "${EXTRAS[@]}"; do [ -f "$EXTRA" ] && source "$EXTRA"; done


# Last takes precedence
EXTRAS=(
  "$HOME/local/texlive/bin/universal-darwin"
  "$HOME/local/bin")

for EXTRA in "${EXTRAS[@]}"; do [ -d "$EXTRA" ] && PATH="$EXTRA":$PATH; done
export PATH


export PAGER=less
export PARINIT="rTbgqR B=.:;,?\!_A_a Q=_s>|"


# Clean-up
unset EXTRA EXTRAS


# Bashrc
[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

