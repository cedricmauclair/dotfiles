# defaults
if [ -f /etc/bashrc ]; then
    source /etc/bashrc
elif [ -f /etc/bash.bashrc ]; then
    source /etc/bash.bashrc
fi


# are we an interactive shell?
if [ "${PS1}" ]; then
    shopt -s checkwinsize
    PS1='[\u@\h \W]$(__git_ps1) \\$ '
else
    return
fi


# enable color support of ls and grep
case "${TERM}" in
    dumb) ;;
    screen)
        [ -f ${HOME}/.dircolors/dircolors.screen ] &&
            eval $(dircolors --sh ${HOME}/.dircolors/dircolors.screen) ;;
    *-color)
        [ -f ${HOME}/.dircolors/dircolors.emacs ] &&
            eval $(dircolors --sh ${HOME}/.dircolors/dircolors.emacs) ;;
    *-256color)
        [ -f ${HOME}/.dircolors/dircolors.256color ] &&
            eval $(dircolors --sh ${HOME}/.dircolors/dircolors.256color) ;;
    *)
        [ -f ${HOME}/.dircolors/dircolors ] &&
            eval $(dircolors --sh ${HOME}/.dircolors/dircolors) ;;
esac


# some ls aliases TODO: use long version of options
alias ls='ls --human-readable --color=auto --time-style=long-iso' # add colors for filetype recognition
alias lx='ls -l --reverse -X'  # sort by extension
alias lk='ls -l --reverse -S'  # list by size
alias la='ls --almost-all'     # show hidden files
alias lla='ls -l --almost-all' # show hidden files
alias lr='ls -l --recursive'   # recursive ls
alias lt='ls -l --reverse -t'  # sort by modification time
alias lm='ls --all -l | more'  # pipe through 'more'
alias ll='ls -l'               # long listing
alias l='ls'                   # quick listing
alias lsd='ls -l | grep "^d"'  #list only directories
alias l.='ls -l --almost-all --directory .*'


# directories aliases
alias ..='cd ..'
alias ...='cd ../..'
alias home='cd ${HOME}/'
alias scratch='cd ${HOME}/scratch'
alias thesis='cd ${HOME}/thesis'
alias memoir='cd ${HOME}/memoir'
alias documents='cd ${HOME}/documents'
alias downloads='cd ${HOME}/downloads'
alias pictures='cd ${HOME}/pictures'
alias pics='cd ${HOME}/pictures'
alias images='cd ${HOME}/pictures'
alias sources="cd ${PREFIX}/sources"
alias acmetex='cd ${HOME}/documents/acmetex'
alias gitrepos="cd ${PREFIX}/gitrepos/cmauclai"
alias dotfiles='cd ${HOME}/dotfiles'


# general aliases
alias setuptex='source ${CONTEXT}/tex/setuptex'
alias current='source ${CONTEXT}/current/tex/setuptex'
alias beta='source ${CONTEXT}/beta/tex/setuptex'
alias em='emacs -nw'
alias findhere='find . -mindepth 1 -maxdepth 1'
alias findupto='find . -mindepth 1 -maxdepth'
alias fgrep='find . -type f -print0 | xargs --null grep'
alias grep='grep --color=auto'
alias mp='ps --forest -alu ${USER}'
alias pf='pathfind PATH'
alias pfa='pathfind --all PATH'


# command substitutions
alias ff='find / -name'
alias df='df --human-readable --exclude-type=tmpfs --exclude-type=usbfs'
alias psg='ps -ef | grep'
alias h='history | grep'
alias which='type -all'
alias ..='cd ..'
alias path="echo -e ${PATH//:/\\\n}"
alias vi='vim'
alias du='du --human-readable'
alias dutop='du --human-readable --max-depth=1'
alias man='LANG=C man'
alias a2ps='LANG=fr_FR.ISO-8859-1 a2ps'
alias genv='env | grep --ignore-case'
alias gpg=gpg2
alias pgp=gpg2
alias metapost='PATH=/DATA/sources/metapost-beta-1.503/build/texk/{web2c,kpathsea}:$PATH mpost'


# makes directory then moves into it
function mkcdr { mkdir -p -v $1; cd $1; }


# creates an archive from given directory
mktar() { tar cvf  "${1%%/}.tar" "${1%%/}"; }
mktgz() { tar cvzf "${1%%/}.tgz" "${1%%/}"; }
mktbz() { tar cvjf "${1%%/}.tbz" "${1%%/}"; }
mkzip() { zip -r   "${1%%/}"     "${1%%/}"; }


# easy extract
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


# alias a directory (default is current)
aliasdir() { alias $1="cd ${2:-${PWD}}"; }


# alias last command
aliascmd() { x=$(history 2 | head -1 | sed 's/.\{7\}//'); alias $1="$x"; }


# BSD specifics
if [ "$(uname)" = "Darwin" ]; then
    alias ls="ls -hG"
fi


# smart completion in shell
if [ -f ${HOME}/.bash_completion ]; then
    source ${HOME}/.bash_completion
elif [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
fi

if [ -d ${HOME}/.bash_completion.d ]; then
    for comp in ${HOME}/.bash_completion.d/*; do
        source ${comp} 2>/dev/null
    done
elif [ -d /etc/bash_completion.d ]; then
    for comp in /etc/bash_completion.d/*; do
        source ${comp} 2>/dev/null
    done
fi


# libraries configurations
export LD_LIBRARY_PATH=${PREFIX}/lib:${LD_LIBRARY_PATH}


# other customization
[ -f "~/.bashrc.$(hostname)" ] && source "~/.bashrc.$(hostname)"


# clean-up
unset MAIL

