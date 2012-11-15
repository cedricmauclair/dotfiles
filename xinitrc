. /etc/X11/Xsession
xrdb -merge "$HOME/.Xresources"
urxvtd &
exec awesome &
setxkbmap fr bepo &
