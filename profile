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

# set show-all-if-ambiguous on


# additions to the ``PATH''
if [ "${HOSTNAME#*.}" = "cert.fr" ]; then
    export PREFIX=/DATA/usr/local
    export TEXLIVE=/DATA/texlive/2010
    export CONTEXT=/DATA/context-minimals/current
elif [ "$(uname)" = "Darwin" ]; then
    export PREFIX=/opt
    export TEXLIVE=${PREFIX}/texlive/2010
    export CONTEXT=${PREFIX}/context-minimals
else
    export PREFIX=/opt
    export TEXLIVE=${PREFIX}/texlive/2010
    export CONTEXT=${PREFIX}/context
fi

# Last takes precedence
[ -d "${TEXLIVE}/bin/i386-linux" ] && pathmunge ${TEXLIVE}/bin/i386-linux
[ -d "${PREFIX}/bin" ]     && pathmunge ${PREFIX}/bin
[ -d "${PREFIX}/scripts" ] && pathmunge ${PREFIX}/scripts
[ -d "${HOME}/bin" ]       && pathmunge ${HOME}/bin


# proxy configurations
if [ "${HOSTNAME#*.}" = "cert.fr" ]; then
    export http_proxy=http://proxy.onecert.fr:80
    export https_proxy=${http_proxy}
    export ftp_proxy=${http_proxy}
    export no_proxy=onera,cert.fr,onecert.fr,127.0.0.1
fi


# viewers/pager/editor configurations
export PDFVIEWER=xpdf
export PSVIEWER=gv
export DVIVIEWER=xdvi

export EDITOR=vim
export VISUAL=view

export OSFONTDIR=${HOME}/.fonts:${TEXLIVE}/../texmf-local/fonts:${TEXLIVE}/texmf-dist/fonts:/usr/share/fonts


# libraries configurations
[ -d "${PREFIX}/lib/pkgconfig"   ] && munge PKG_CONFIG_PATH ${PREFIX}/lib/pkgconfig
[ -d "${PREFIX}/share/pkgconfig" ] && munge PKG_CONFIG_PATH ${PREFIX}/share/pkgconfig
[ -d "${PREFIX}/lib/python2.6"   ] && munge PYTHONPATH ${PREFIX}/lib/python2.6/site-packages
[ -d "${PREFIX}/lib/perl5"       ] && munge PERL5LIB ${PREFIX}/lib/perl5/site_perl/5.12.3
export PKG_CONFIG_PATH PYTHONPATH PERL5LIB

# Platform specific settings
if [ "${HOSTNAME#*.}" = "cert.fr" ]; then
    export TERM=rxvt-unicode-256color
    export PAGER=most
    [ -d "/DATA/opt/google/chrome" ] && pathmunge /DATA/opt/google/chrome
    export RSYNC_CONNECT_PROG="ssh home nc %H 873"
elif [ "$(uname)" = "Darwin" ]; then
    export TERM=rxvt
    export PAGER=less
else
    export TERM=rxvt-unicode-256color
    export PAGER=most
fi


# clean-up
unset pathmunge
unset munge


# some more definitions
source ${HOME}/.bashrc

