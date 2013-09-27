# Defaults
[ -f /etc/bashrc ]      && source /etc/bashrc
[ -f /etc/bash.bashrc ] && source /etc/bash.bashrc 

# Are we an interactive shell?
if [ ! "${PS1}" ]; then return; fi

shopt -s checkwinsize
PS1='[\u@\h \W]$(__git_ps1) \\$ '

# Some ls aliases
alias ls='\ls --human-readable --color=auto --time-style=long-iso' # default settings,
alias tree='\tree -Cs'      # nice alternative 
alias ll='ls -l'            # long listing
alias lx='ll -X'            # sort by extension
alias lt='ll -t --reverse'  # sort by reversed date
alias lk='ll -S --reverse'  # list by reversed size
alias la='ls --almost-all'  # show hidden files
alias lla='la -l'           # show hidden files
alias lr='ll --recursive'   # recursice ls
alias lm='lla | less'       # pipe through 'less'
alias l='ls'                # quick listing
alias lsd='ll | \grep "^d"' # list directories only
alias l.='lla -d .*'        # list directories only

# Directories aliases
alias ..='cd ..'
alias home='cd ${HOME}'
alias documents='cd ${HOME}/documents'
alias downloads='cd ${HOME}/downloads'
alias pictures='cd ${HOME}/pictures'

# General aliases
alias em='emacs -nw'
alias findhere='find . -mindepth 1 -maxdepth 1'
alias findupto='find . -mindepth 1 -maxdepth'
alias fgrep='find . -type f -print0 | xargs --null grep'
alias grep='\grep --color=auto'
alias mp='ps --forest -alu ${USER}'

# Command substitutions
ff() { find / "$@" 2>/dev/null; }
alias df='df --human-readable -x tmpfs -x usbfs'
alias h='history | grep'
alias which='type -all'
alias path='echo -e ${PATH//:/\\\n}'
alias vi='vim'
alias du='du --human-readable'
alias dutop='du --human-readable --max-depth=1'
alias luarocks="\luarocks --local"

# Makes directory then moves into it
mkcdr() { mkdir -p -v $1; cd $1; }

# Creates an archive from given directory
mktar() { tar cvf  "${1%%/}.tar" "${1%%/}"; }
mktgz() { tar cvzf "${1%%/}.tgz" "${1%%/}"; }
mktbz() { tar cvjf "${1%%/}.tbz" "${1%%/}"; }
mkzip() { zip -r   "${1%%/}"     "${1%%/}"; }

# Easy extract
extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf $1    ;;
            *.tar.gz)    tar xvzf $1    ;;
            *.bz2)       bunzip2 $1     ;;
            *.rar)       rar x $1       ;;
            *.gz)        gunzip $1      ;;
            *.tar)       tar xvf $1     ;;
            *.tgz)       tar xvzf $1    ;;
            *.tbz2)      tar xvjf $1    ;;
            *.tbz)       tar xvjf $1    ;;
            *.zip)       unzip $1       ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1        ;;
            *)           echo "don't know how to extract '$1'..." ;;
        esac
    else
        echo "'$1' is not a valid file!"
    fi
}

# Alias a directory (default is current)
aliasdir() { alias $1="cd ${2:-${PWD}}"; }

# Alias last command
aliascmd() { x=$(history 2 | head -1 | sed 's/.\{7\}//'); alias $1="$x"; }

# Other customization
EXTRAS=(
  /etc/bash_completion
)

for EXTRA in "${EXTRAS[@]}"; do [ -f "$EXTRA" ] && source "$EXTRA"; done

# clean-up
unset MAIL MAILCHECK EXTRA EXTRAS

[ -f "$HOME/.bashrc.$(uname)" ] && source "$HOME/.bashrc.$(uname)"
[ -f "$HOME/.bashrc.$(hostname)" ] && source "$HOME/.bashrc.$(hostname)"

