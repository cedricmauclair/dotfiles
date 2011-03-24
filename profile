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


# no core files by default
ulimit -S -c 0 >/dev/null 2>&1


# history configurations
export HISTSIZE=2000
export HISTFILESIZE=2000
export HISTIGNORE="cd:ls"
export HISTCONTROL=ignoreboth:erasedups
export PROMPT_COMMAND="history -a; ${PROMPT_COMMAND}"

set show-all-if-ambiguous on


# additions to the ``PATH''
export PREFIX=/opt

if [ "${HOSTNAME#*.}" = "cert.fr" ]; then
    PREFIX=/DATA/usr/local
fi

[ -d "/DATA/texlive/2010/bin/i386-linux" ] && pathmunge /DATA/texlive/2010/bin/i386-linux
[ -d "/DATA/opt/google/chrome" ]           && pathmunge /DATA/opt/google/chrome

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
export PAGER=most

export OSFONTDIR=/usr/share/fonts:/DATA/texlive/2010/texmf-dist/fonts:${HOME}/.fonts


# libraries configurations
if [ "${HOSTNAME#*.}" = "cert.fr" ]; then
    export PKG_CONFIG_PATH=${PREFIX}/lib/pkgconfig:${PKG_CONFIG_PATH}
    export PYTHONPATH=${PREFIX}/lib/python2.6/site-packages
    export PERL5LIB=${PREFIX}/lib/perl5/site_perl/5.12.3:${PERL5LIB}
fi


# clean-up
unset pathmunge


# some more definitions
source ${HOME}/.bashrc
