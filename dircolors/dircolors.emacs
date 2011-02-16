# Configuration file for the color ls utility
# This file goes in the /etc directory, and must be world readable.
# You can copy this file to .dir_colors in your $HOME directory to
# override the system defaults.

# COLOR needs one of these arguments: 'tty' colorizes output to ttys, but
# not pipes.  'all' adds color characters to all output.  'none' shuts
# colorization off.
COLOR tty
OPTIONS -F

# Below, there should be one TERM entry for each termtype that is
# colorizable
TERM eterm-color
TERM screen

# EIGHTBIT, followed by '1' for on, '0' for off.  (8-bit output)
EIGHTBIT 1

# Below are the color init strings for the basic file types.  A color init
# string consists of one or more of the following numeric codes:
# Attribute codes:
# 00=none 01=bold 04=underscore 05=blink 07=reverse 08=concealed
# Text color codes:
# 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
# Background color codes:
# 40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan 47=white
NORMAL  00      # global default, although everything should be something.
FILE    00      # normal file
DIR     01      # directory
LINK    00;36   # symbolic link
FIFO    00;37   # pipe
SOCK    40;35   # socket
BLK     33;01   # block device driver
CHR     33;01   # character device driver
ORPHAN  40;31;01    # symlink to nonexistent file, or non-stat'able file
MISSING 01;05;37;41 # ... and the files they point to
SETUID  37;41       # file that is setuid (u+s)
SETGID  30;43       # file that is setgid (g+s)
CAPABILITY 30;41    # file with capability
STICKY_OTHER_WRITABLE 30;42 # dir that is sticky and other-writable (+t,o+w)
OTHER_WRITABLE 34;42 # dir that is other-writable (o+w) and not sticky
STICKY  37;44       # dir with the sticky bit set (+t) and not other-writable

# This is for files with execute permission:
EXEC 00;32

# List any file extensions like '.gz' or '.tar' that you would like ls
# to colorize below.  Put the extension, a space, and the color init
# string.  (and any comments you want to add after a '#')
.tar 01;33 # archives or compressed
.tgz 01;33
.tbz 01;33
.ace 01;33
.arj 01;33
.taz 01;33
.lzh 01;33
.rar 01;33
.zip 01;33
.7z  01;33
.z   01;33
.Z   01;33
.bz2 01;33
.gz  01;33
# image formats
.jpg 01;33
.gif 01;33
.bmp 01;33
.pbm 01;33
.pgm 01;33
.ppm 01;33
.tga 01;33
.xbm 01;33
.xpm 01;33
.tif 01;33
.tiff 01;33
.png 01;33
.mng 01;33
.pcx 01;33
.mov 01;33
.mpg 01;33
.mpeg 01;33
.m2v 01;33
.mkv 01;33
.ogm 01;33
.mp4 01;33
.m4v 01;33
.mp4v 01;33
.vob 01;33
.qt  01;33
.nuv 01;33
.wmv 01;33
.asf 01;33
.rm  01;33
.rmvb 01;33
.flc 01;33
.avi 01;33
.fli 01;33
.flv 01;33
.gl 01;33
.dl 01;33
.xcf 01;33
.xwd 01;33
.yuv 01;33
.svg 01;33
.svgz 01;33
.axv 01;33
.anx 01;33
.ogv 01;33
.ogx 01;33
.eps 00;33
.pdf 00;33
# audio formats (cyan)
.aac 01;36
.au 01;36
.flac 01;36
.mid 01;36
.midi 01;36
.mka 01;36
.mp3 01;36
.mpc 01;36
.ogg 01;36
.ra 01;36
.wav 01;36
# http://wiki.xiph.org/index.php/MIME_Types_and_File_Extensions
.axa 01;36
.oga 01;36
.spx 01;36
.xspf 01;36
