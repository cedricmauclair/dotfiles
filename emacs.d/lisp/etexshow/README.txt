Dear etexshow users:

Installation
============

First I'd like to give some installation instructions. Installing
etexshow consists of the following steps:

1) Tell emacs where to find etexshow
2) Optionally byte-compile the file
3) Tell etexshow where to find the xml-files with the commands and
4) Tell etexshow where to find the comment-file.


1) Tell emacs where to find etexshow

When emacs tries to find files that end with ".el" (like etexshow), it
looks in all paths that are mentioned in the (emacs) variable
`load-path'. To show the value of the `load-path' variable, run emacs
and press `C-h v load-path <RETURN>'. It will give you an output like

----------------------------------------
load-path's value is 

("~/elisp/etexshow" "/opt/local/elisp/" "/opt/emacs/21/share/emacs/21.1/lisp"
 "/opt/emacs/21/share/emacs/21.1/lisp/eshell" 
 "/opt/emacs/21/share/emacs/21.1/lisp/emulation" ...)
----------------------------------------

So you have two posibilites: put etexshow (and the other ".el" files
that come with etexshow) in a directory that is already serched by
emacs or put them in a (perhaps new) directory and add this new
directory to the load-path. Say you make a directory called
"/opt/emacs/etexshow" and copy all .el-files to this directory. Then
you have to instruct emacs to search this directory:

(add-to-list 'load-path "/opt/emacs/etexshow")

Put this line into an emacs-startup file. It is either the file

".emacs" in your home directory (In Windows the .emacs file is
     normally found at the root of the hard disk with the smallest
     letter, most commonly C:\.emacs). Use this if you want to change
     it on a per user basis. or

"site-start.el" in ".../share/emacs/site-lisp/" subdirectory in your
     emacs installation. Use this for system-wide installations.


And tell emacs to `autoload' the etexshow function. If you have done
this, etexshow can be used by typing `M-x etexshow <RETURN>' in
emacs. This is how to autoload etexshow:

(autoload 'etexshow "etexshow"  "Browser for ConTeXt commands." t)

(Put this in the file where you have set the load-path variable.)

XEmacs needs the fsf-compat package in order to work. I make use of
overlays and thing-at-pt, this is not in vanilla XEmacs.


2)  Optionally byte-compile the file

To be documented later. Brave users use:
emacs -q -L . -batch -f batch-byte-compile etexshow.el pgxml.el
or
xemacs -batch -eval "(add-to-list 'load-path \".\")" -f batch-byte-compile  pgxml.el etexshow.el overlay.el
on the command line in the etexshow directory.


3) Tell etexshow where to find the xml-files with the commands

Etexshow has to find an xml file with all context commands. With the
ConTeXt versions starting Feb 2003 there is a high chance to be able
to generate such an xml file with `texexec'ing setupe.tex (that is
included in the ConTeXt distribution) and use the resulting
`cont-en.xml' file as an input file for etexshow. You can use the
supplied cont-en.xml file. Be aware that it might be outdated. 

To speed up things, etexshow generates a cache file. Parsing the xml
file takes a very long time. So etexshow puts the data in a more
efficient structure and writes it to a temporary file. To tell
etexshow about the two files use an instruction like this:

(setq etexshow-xml-files-alist 
      '(("/etexshow/cont-en.xml" . "/tmp/cont-en.cache")))

Please change the paths that match your system requirements. Again,
use the ".emacs" or "site-start.el" for setting this option. Make sure
that etexshow can write into the directory that is mentioned in the
second part of the above statement. If the cache file (here:
cont-en.cache) is outdated or missing, it will be automatically
created.

For the advanced users: you can have multiple xml files. For example,
use the official cont-en.xml and a local mycommands.xml. Now this
should be setup like:

    (setq etexshow-xml-files-alist 
          '(("/etexshow/cont-en.xml" . "/etexshow/cont-en.cache")
            ("/etexshow/mycommands.xml" . "/etexshow/mycommands.cache")))



4) Tell etexshow where to find the comment-file

You can add comments to the commands. These will be saved in the file
that is configured in `etexshow-comment-file'. To change this, set
something like 

(setq etexshow-comment-file   "~/.cont-en-comments.xml" )

in your emacs startup file.

Example. This is what I have in my .emacs file:
--------------------------------------------------

(add-to-list 'load-path "~/elisp/etexshow")
(autoload 'etexshow "etexshow"  "Browswer for ConTeXt commands." t)

(setq etexshow-xml-files-alist 
      '(("~/elisp/etexshow/cont-en.xml" . 
 	 "~/elisp/etexshow/cont-en.cache")
 	("~/elisp/etexshow/mycommands.xml" . 
 	 "~/elisp/etexshow/mycommands.cache")))

(global-set-key [f7] 'etexshow)
(add-hook 'etexshow-mode-hook '(lambda () 
				 (local-set-key [f7] 'etexshow-quit)))
--------------------------------------------------

With this setup: pressing F7 runs etexshow and pressing F7 again (from within
etexshow) will quit etexshow.



Using etexshow
==============

to be documented later. See etexshow.el or the help page for now. 



Thanks to Mari Voipio and Simon Pepping for comments. 

..................................
Please send any kind of comments to patrick@gundla.ch

Bochum, Germany, 2004/04/11
