# from ``/etc/profile'' on Fedora 11
pathmunge () {
    if ! echo $PATH | egrep -q "(^|:)$1($|:)" ; then
       if [ "$2" = "after" ] ; then
          PATH=$PATH:$1
       else
          PATH=$1:$PATH
       fi
    fi
}

munge () {
    if ! echo \$$1 | egrep -q "(^|:)$2($|:)" ; then
       if [ "$3" = "after" ] ; then
          eval $1=\$$1:$2
       elif [ "$3" = "export" ] ; then
          eval export $1=\$$1:$2
       else
          eval $1=$2:\$$1
       fi
    fi
}


# no core files by default
ulimit -S -c 0 >/dev/null 2>&1


# history configurations
export HISTSIZE=2000
export HISTFILESIZE=2000
export HISTIGNORE=cd:ls
export HISTCONTROL=ignoreboth:erasedups
export PROMPT_COMMAND="history -a; ${PROMPT_COMMAND}"


# Useful variables
export PREFIX=~

[ -d "${PREFIX}/texlive" ] && export TEXLIVE=${PREFIX}/texlive
[ -d "${PREFIX}/context" ] && export CONTEXT=${PREFIX}/context


# PATH additions, last takes precedence
[ -d "${TEXLIVE}/bin/i386-linux" ] && pathmunge ${TEXLIVE}/bin/i386-linux
[ -d "${PREFIX}/bin" ]     && pathmunge ${PREFIX}/bin
[ -d "${HOME}/bin" ]       && pathmunge ${HOME}/bin


# viewers/pager/editor configurations
export PDFVIEWER=xpdf
export PSVIEWER=gv
export DVIVIEWER=xdvi

export EDITOR=vim
export VISUAL=view

export OSFONTDIR=${HOME}/.fonts:${TEXLIVE}/../texmf-local/fonts:${TEXLIVE}/texmf-dist/fonts:/usr/share/fonts


# Platform specific settings
[ -f "~/.profile.$(hostname)" ] && source "~/.profile.$(hostname)"


# clean-up
unset pathmunge
unset munge


# some more definitions
source ${HOME}/.bashrc

startx && exit 0

