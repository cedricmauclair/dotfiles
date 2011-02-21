;;; etexshow.el --- Browser for ConTeXt commands.

;; Last Change: Sun Apr 11 15:30:16 2004


;; Copyright (C) 2003 Patrick Gundlach

;; Author: Patrick Gundlach <patrick@gundla.ch>
;; Created: 07 Jan 2003

(defconst etexshow-version "Alpha 5c"
  "Version of etexshow. Display with `etexshow-version'")

;; Keywords: tex
;; Homepage: http://levana.de/emacs/


;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; http://www.fsf.org/copyleft/gpl.html 


;;; Commentary:

;;  see also help-file 

;; Etexshow is a clone of
;; texshow, a program shipped with the ConTeXt macro package (see
;; pragma-ade.com for a description of ConTeXt). It will serve as the base
;; of a context-mode for emacs. Please send bug reports and comments to me!
;; Thank you. See file todo for things that need to be done (that I know
;; of).

;; There is still an xml-file shipped with this code. Usually you would
;; generate the xml-file with 'texexec'ing the file setupe.tex. Then you
;; will get the cont-en.xml file that can (could) be used as an input for
;; this etexshow. But for now, this won't work. It will work rsn.

;;   Installation
;; ================

;; please find installation instructions in the accompanied file README.txt


;;   Startup 
;; ===========

;; To start etexshow, just
;; type 'M-x etexshow'
;; 
;; WARNING: The first start: etexshow reads an xml file and converts it
;; (pgxml.el) to a datastructure that will be stored in a cache file.
;; Please note that will take some time. 


;;   Configuration
;; =================

;; Etexshow works for me. Hopefully it will work for you as well. But
;; there are some points you have to look at. Etexshow will try to
;; find the xml files that is given in the alist
;; `etexshow-xml-files-alist'. Then it will write to a cache file
;; (which must not exist!) that are also given in this alist. Please
;; put a line in your .emacs where to find (store) these files:

;; (setq etexshow-xml-files-alist 
;;       '(("~/etexshow/cont-en.xml"    . "~/etexshow/cont-en.cache")
;; 	   ("~/etexshow/mycommands.xml" . "~/etexshow/mycommands.cache")))

;; (the dots inbetween are needed). You have to give an even number of
;; files. The first file is the xml file, and the second file is the
;; cache file.
 
;; The colors are tested on black background. On any other background
;; they will probably look ugly. I said "ugly". Please give me your
;; favourite color definitions (look for the defface arguments)


;;   Functions
;; =============
;; You will see a list of commands on the left side and an explanation
;; on the right hand side. Use cursor up/down to scroll thru the list.

;; Please see the help page for more information.

;;   Exit
;; ========
;; To exit etexshow please use 'Q' (uppercase letter q)

;;; Code:

(require 'pgxml); the xmlparser from emacs 21, slightly patched


;; to compile under gnu-emacs 21 without warning messages:
(unless (featurep 'xemacs)
  (defalias 'defvaralias 'ignore)
  (defvar modeline-format nil))

(when (featurep 'xemacs) ; xemacs has extents, no overlays, so I have
  (require 'overlay)	 ; to use the compatibilty package
  (require 'thingatpt)
  (defvaralias 'last-nonmenu-event 'current-mouse-event)) ; safe?


(defvar etexshow-optional 'underline
  "How to display optional arguments. Example: 'underline or 'italic.")
(defvar etexshow-default 'underline
  "How to display default arguments. Example: 'underline or 'italic.")
(defvar etexshow-variable 'bold
  "How to display variable parts. Example: 'bold or 'italic.")


(defface etexshow-first-arg 
  `((((class color) (background dark))
     (:foreground "lawn green"))
    (((class color) (background light))
     (:foreground "dark green")))
  "Face for first argument.")

(defface etexshow-second-arg 
  `((((class color) (background dark))
     (:foreground "magenta"))
    (((class color) (background light))
     (:foreground "dark blue")))
  "Face for second argument.")


(defface etexshow-third-arg 
  `((((class color) (background dark))
     (:foreground "lightblue"))
    (((class color) (background light))
     (:foreground "black")))
  "Face for third argument.")

(defface etexshow-fourth-arg 
  `((((class color) (background dark))
     (:foreground "deep pink"))
    (((class color) (background light))
     (:foreground "red") ))
  "Face for fourth argument.")

(defvar etexshow-arg-faces 
  '(etexshow-first-arg 
    etexshow-second-arg
    etexshow-third-arg
    etexshow-fourth-arg)
  "List of faces to be used for coloring the inserted arguments and
explanations." )  


;; only one of these two is needed. Decide!
(defmacro etexshow-withface  (thisface &rest body)
  "All inserts in BODY will have the face FACE."
  (let ((begin (make-symbol "begin"))
	(face (make-symbol "face")))
    `(let ((,begin (point))
	   (,face ,thisface))
       ,@body
       (add-text-properties ,begin (point) 
			    (list 'face ,face)))))

(defmacro etexshow-withface-count  (thiscount &rest body)
  "All inserts in BODY will have the face FACE."
  (let ((begin (make-symbol "begin"))
	(count (make-symbol "count")))
    `(let ((,begin (point))
	   (,count ,thiscount))
       ,@body
       (add-text-properties ,begin (point) 
			    (list 'face 
				  (nth (mod ,count 
					    (length etexshow-arg-faces))
				       etexshow-arg-faces))))))

(defvar etexshow-preferred-column 19
  "Column the explanations will start if the left side is not too large")
(defvar etexshow-min-dist 2
  "Distance between the left side and the right side XXX.")
(defvar etexshow-current-startcmd "start"
  "The language-dependant \\start...\\stop command.")

(defvar etexshow-comment-formatstring "\n----comment (%s)----\n"
  "Format string for comments. Language should go into parentheses")
(defvar etexshow-comment-regexp "----comment (\\(\\w\\w\\))----\n"
  "Matches separator for comments.")

(defvar etexshow-cursor-overlay nil 
  "Cursor'bar'")

(defface etexshow-cursor-face 
       `((((type tty) (class color))
          (:background "blue" :foreground "white"))
         (((type tty) (class mono))
          (:inverse-video t))
         (((class color) (background dark))
	  (:background "blue" :foreground "white"))
	 ;;(:background "blue" :foreground "white"))
         (((class color) (background light))
	  (:background "blue" :foreground "white"))
	 ;;(:background "lightblue"))
         (t (:background "gray")))
       "Basic face for highlighting the region."
       :group 'basic-faces)

;; (defface etexshow-cursor-face 
;;   `((((class color) (background dark))
;;      (:background "blue" :foreground "white"))
;;     (((class color) (background light))
;;      (:background "blue" :foreground "white" )))
;;   "Face for first argument.")


;; command buffer:
(defvar etexshow-cmd-buffer-name "Commands"
  "name of the command buffer")
(defvar etexshow-cmd-buffer nil
  "buffer object of command buffer")

;; description buffer
(defvar etexshow-description-buffer-name "Description"
  "name of the buffer with the descriptions")
(defvar etexshow-description-buffer nil
  "buffer object of description buffer")
(defvar etexshow-help-buffer-name "etexshow help"
  "Name for help buffer")
(defvar etexshow-help-buffer nil
  "buffer object for help")

;; data structures
(defvar etexshow-commands-hasht nil
  "Hash table for all commands. Key is internal name.")
(defvar etexshow-comments-hasht nil
  "Hash table for all comments. Key is internal name.")
(defvar etexshow-commands-vec nil
  "Vector holding diplayed command in command buffer. Line/internal
command name is stored.")
(defvar etexshow-commands-count 0
  "Number of commands shown in command buffer.")
(defvar etexshow-commands-count-string ""
  "String that holds `etexshow-commands-count'. Only for xemacs
compatibilty (used in modeline).")

;; perhaps make a symbol of these?
(defvar etexshow-language-filter '("en" "de")
  "Accept comments in these languages")
(defvar etexshow-comment-language "en"
  "The shorthand for the current interface, as en, nl, de,...")


;; hyperlinks
(defvar etexshow-hyperlink-alist nil
  "Alist that holds the hyperlinks for current command")
(defvar etexshow-hyperlink-marker-alist nil
  "Alist that holds the position for displayed references")
(defvar etexshow-hyperlink-alist-alist nil
  "This crazy datastructure stores the alist at a certain insertion level")
(defvar etexshow-end-description-marker nil
  "Marks the end of the description in current buffer")


(defvar etexshow-all-commands nil
  "List of all (internal) commands")

;; xml stuff
(defvar etexshow-xml-files-alist
  '(("~/etexshow/cont-en.xml" . "~/etexshow/cont-en.cache")
    ("~/etexshow/mycommands.xml" . "~/etexshow/mycommands.cache"))
  "alist of xml-files and cache-files for the context commands.")

(defvar etexshow-comment-file "~/.cont-en-comments.xml"
  "File that holds comments and explanations for etexshow-commands")
(defvar etexshow-comment-buffer nil
  "Buffer object for comment-buffer.")
(defvar etexshow-keep-comment-buffer nil
  "Keep comment buffer. When non nil, etexshow will not delete the
comment buffer after quitting.")

(defvar etexshow-xml-top-node nil
  "Pointer to the first (and only) tag in the xml file")
(defvar etexshow-current-filter-string ""
  "Commands are only shown if they contain a string like this one")
(defvar etexshow-current-isearch-string nil
  "Used for isearch functionality. Holds the current string to be 
searched for.")
(defvar etexshow-command-history nil
  "List of commands the user has visited.")



;;; key binding stuff
(defvar etexshow-cmd-buffer-map (make-sparse-keymap)
  "keymap for command buffer")

(defvar etexshow-des-buffer-map (make-sparse-keymap)
  "keymap for description buffer")

;; etexshow-isearch 
(substitute-key-definition ;; this is crazy!!!
 'self-insert-command 
 'etexshow-isearch etexshow-cmd-buffer-map global-map)

;; somehow xemacs 21.
(define-key etexshow-cmd-buffer-map [(tab)] 'ignore)

(define-key etexshow-cmd-buffer-map [up] '(lambda ()
					    (interactive)
					    (etexshow-next-line -1)))
(define-key etexshow-cmd-buffer-map [down] 'etexshow-next-line)
(define-key etexshow-cmd-buffer-map [?1] 'etexshow-toggle-inherit-reference)
(define-key etexshow-cmd-buffer-map [?2] 'etexshow-toggle-inherit-reference)
(define-key etexshow-cmd-buffer-map [?3] 'etexshow-toggle-inherit-reference)
(define-key etexshow-cmd-buffer-map [?4] 'etexshow-toggle-inherit-reference)
(define-key etexshow-cmd-buffer-map [?5] 'etexshow-toggle-inherit-reference)
(define-key etexshow-cmd-buffer-map [?6] 'etexshow-toggle-inherit-reference)
(define-key etexshow-cmd-buffer-map [?7] 'etexshow-toggle-inherit-reference)
(define-key etexshow-cmd-buffer-map [?8] 'etexshow-toggle-inherit-reference)
(define-key etexshow-cmd-buffer-map [?9] 'etexshow-toggle-inherit-reference)
(define-key etexshow-cmd-buffer-map [?B] 'etexshow-back-history)
(define-key etexshow-cmd-buffer-map [?C] 'etexshow-comment-command)
(define-key etexshow-cmd-buffer-map [?D] 'etexshow-delete-comment)
(define-key etexshow-cmd-buffer-map [?H] 'etexshow-help)
(define-key etexshow-cmd-buffer-map [??] 'etexshow-help)
(define-key etexshow-cmd-buffer-map [?J] 'etexshow-goto-reference)
(define-key etexshow-cmd-buffer-map [?L] 'etexshow-toggle-comment-language)
(define-key etexshow-cmd-buffer-map [?Q] 'etexshow-quit)
(define-key etexshow-cmd-buffer-map [?R] '(lambda () (interactive) (etexshow-isearch-again t)))
(define-key etexshow-cmd-buffer-map [?S] 'etexshow-isearch-again)
(define-key etexshow-cmd-buffer-map [?V] 'etexshow-version)
(define-key etexshow-cmd-buffer-map [(backspace)]  'etexshow-delete-isearch-char)
(define-key etexshow-cmd-buffer-map [?/] 'etexshow-filter)
(define-key etexshow-cmd-buffer-map [?*] 'etexshow-un-filter)
(define-key etexshow-cmd-buffer-map [(return)] 
  '(lambda () (interactive)
     (etexshow-show-current-description t)))

(define-key etexshow-cmd-buffer-map [home] '(lambda () (interactive)
					      (etexshow-goto-command 1)))
(define-key etexshow-cmd-buffer-map [end] '(lambda () (interactive)
 					     (etexshow-goto-command 
					      etexshow-commands-count)))

;; mosemovement has different events in xemacs
(if (featurep 'xemacs)
    (progn
      (define-key etexshow-cmd-buffer-map [button1] 'etexshow-show-mouse)
      (define-key etexshow-cmd-buffer-map [button5] 
	'(lambda () (interactive) (etexshow-scroll-down-some-files 1)))
      (define-key etexshow-cmd-buffer-map [button4] 
	'(lambda () (interactive) (etexshow-scroll-down-some-files
				   -1)))
      (define-key etexshow-des-buffer-map [button1] 
	'etexshow-toggle-inherit-mouse)
      (define-key etexshow-des-buffer-map [button2] 
	'etexshow-goto-reference-mouse)
      )
  
  (define-key etexshow-cmd-buffer-map [mouse-1] 'etexshow-show-mouse)
  (define-key etexshow-cmd-buffer-map [mouse-5] 
    '(lambda () (interactive) (etexshow-scroll-down-some-files 1)))
  (define-key etexshow-cmd-buffer-map [mouse-4] 
    '(lambda () (interactive) (etexshow-scroll-down-some-files -1)))
  (define-key etexshow-des-buffer-map [down-mouse-1] 'ignore)
  (define-key etexshow-des-buffer-map [double-mouse-1] 
    'etexshow-toggle-inherit-mouse)
  (define-key etexshow-des-buffer-map [mouse-1] 
    'etexshow-toggle-inherit-mouse)
  (define-key etexshow-des-buffer-map [mouse-2] 
    'etexshow-goto-reference-mouse)

)
  


(define-key etexshow-cmd-buffer-map [prior] 
  '(lambda () (interactive) (etexshow-scroll-down-some-files -1)))

(define-key etexshow-cmd-buffer-map [next] 
  '(lambda () (interactive) (etexshow-scroll-down-some-files 1)))



(define-key etexshow-des-buffer-map [(control ?c)(control ?c)] 
  'etexshow-save-comment)
(define-key etexshow-des-buffer-map [(control ?c)(control ?d)] 
  'etexshow-delete-comment)

;; probably not needed. wanted to make sure that etexshow-quit will
;; restore the windows correctly. I will probably to this by simply
;; deleting the description window and kill both buffers. See
;; etexshow-quit for details
(defvar etexshow-before-window-configuration nil
  "Window-configuration before startup")

(defvar etexshow-window-configuration nil
  "Window-configuration after startup")



;;; real code starting here




;; we have two data structures.
;; 1: vector that holds all commands that are shown in the
;;    command-buffer: lineno / internalname.
;; 2: hashtable with the commands.
(defun etexshow (&optional prefix)
  "Make a new buffer and shows syntax of the command."
  (interactive "P")
  ;; we only need to fill the hash table and the commands-list when
  ;; run the first time or with prefix argument.
 
  (etexshow-init)

  (when (or prefix
	    (not (hash-table-p etexshow-commands-hasht)))
    (etexshow-prepare-datastructures))

  (etexshow-prepare-comment-buffer "en")
  (etexshow-un-filter)

  ;; we display at most (length of hashtable) commands
  ;; All displayed commands in the command buffer will be stored in
  ;; this vector. Filled in etexshow-fill-commands

  (etexshow-fill-commands) ; cmd buffer
  (pop-to-buffer etexshow-cmd-buffer)
  (run-hooks 'etexshow-mode-hook)
  (etexshow-show-current-description t)
  (message "Welcome to etexshow. Press `Q' to quit or `H' for help."))

;; not documented 
(defun etexshow-cmd (&optional cmdname)
  (interactive)
  (unless cmdname
    ;; in xemacs, thing-at-point is in fsf-compat/thingatpt.el
    (let ((csatpoint (thing-at-point 'word)))
      ;; avoid substring-no-properties because emacs 21.3.1 does not have it
      (when csatpoint 
	(set-text-properties 0 (length csatpoint) nil csatpoint)
	(setq cmdname csatpoint	))))
  (etexshow)
  (when cmdname
    (set-buffer etexshow-cmd-buffer)
    (goto-char (point-min))
    (let ((foundpoint nil))
      (when (re-search-forward
	     (concat 
	      (regexp-quote (concat "\\" cmdname)) "$") nil t)
	(setq foundpoint (point)))
      (when foundpoint
	(goto-char foundpoint)
	(beginning-of-line)
	(etexshow-show-current-description)))))


(defun etexshow-quit ()
  "Destroy the buffers and windows if they are still alive."
  (interactive)

;; this works in xemacs: :-(
;;   (set-buffer etexshow-cmd-buffer)
;;   (delete-window)
;;   (kill-buffer etexshow-cmd-buffer)

;;   (set-buffer etexshow-description-buffer)
;;   (delete-window)
;;   (kill-buffer etexshow-description-buffer))


  (mapcar  '(lambda (buf)
	      (when (buffer-live-p buf)
		(set-buffer buf)
		;; buffer-display-count not in xemacs!
		;; it is set even when not visible.
		;; check if the following works fine... 
;;		(if buffer-display-count
		(unless (one-window-p)
		  (delete-window))
;;		    )
		(kill-buffer buf)))
	   `(,etexshow-cmd-buffer ,etexshow-description-buffer))



;;   (when (buffer-live-p etexshow-cmd-buffer)
;;     (when (window-live-p (get-buffer-window etexshow-cmd-buffer))
;;       (delete-window (get-buffer-window etexshow-cmd-buffer)))
;;     (kill-buffer etexshow-cmd-buffer))
;;   (when (buffer-live-p etexshow-description-buffer)
;;     (when (window-live-p (get-buffer-window etexshow-description-buffer))
;;       (delete-window (get-buffer-window etexshow-description-buffer)))
;;     (kill-buffer etexshow-description-buffer))

  (when (and (buffer-live-p etexshow-comment-buffer)
	     (not etexshow-keep-comment-buffer))
    (kill-buffer etexshow-comment-buffer))
;;  (set-window-configuration etexshow-before-window-configuration)
  )


(defun etexshow-version ()
  "Print out version information for etexshow."
  (interactive)
  (message "etexshow version: %s" etexshow-version))

(defun etexshow-current-internal-name ()
  "Return internal name where cursor is on"
  (save-excursion
    (set-buffer etexshow-cmd-buffer)
    (aref etexshow-commands-vec
	  (save-excursion
	    (beginning-of-line)
	    (count-lines (point-min) (point))))))

(defun etexshow-find-equal (item sequence)
  "Find element ITEM in sequence SEQUENCE and return position or nil
if not available using equal as test." 
  (cond ((= (length sequence) 0)
	 nil)
	((equal item (car sequence))
	 0)
	(t (let ((ret (etexshow-find-equal item (cdr sequence))))
	     (if ret 
		 (1+ ret)
	       nil)))))


(defun etexshow-help ()
  "Display help in description buffer."
  (interactive)
  (pop-to-buffer etexshow-description-buffer)
  (setq etexshow-help-buffer 
	(switch-to-buffer
	 (get-buffer-create etexshow-help-buffer-name)))
  
  (insert "\
Help File for etexshow - please send any comments
                         to patrick@gundla.ch

Overview of key-bindings
------------------------
+ all keys that would normally be inserted into a
  buffer are bound to the etexshow-isearch command

+ Keys in combination with the shift-key:

  B    Jumps back to the last hyperlink
  C    Inserts a comment for the current command
  D    Deletes the comment for the current command
  H,?  This help screen
  J    Follow cross reference
  L    Toggle default language for comments
  Q    Quit etexshow
  R    Repeat last etexshow-isearch backward
  S    Repeat last etexshow-isearch forward
  V    Display etexshow information
  /    Creates a filter. Display only commands
        that match against this string. 
  *    Remove filter.
  1-9  Toggle the display of a cross reference

  <backspace>           deletes last isearch character
  <page-up>/<page-down> scroll up/down one screen
  <home> / <end>        first/last command


+ Mouse bindings

  Left mouse button	displays the selected
       	     		command
  Wheel mouse (button 4 and 5)
  	      	      	scroll up/down one screen

Searching and filtering
-----------------------
To find your favourite command as quick as
possible, there are several mechanisms built into
etexshow. You can, of course, use the cursor keys
to move up/down in the list of the commands. Page
up/down will jump backward/forward one screen
height. Home/end will display the first/last
command in the buffer.

You can use the isearch facility: type some letters
and the cursor will move to the first entry that
matches the string. Say you are at the beginning of
the commands buffer. Type in `c' will jump to
\\background, since this is the first command that
has the letter `c' in it. Pressing `k' will stay
there and the key `r' will now move to \\blackrule.
If you want to find the next match you can press
`S' (search again) or `R' for backward searching.

To restrict the number of commads displayed in the
command buffer, use the filtering mechanism. Press
`/' to enable the filter and answer the
question. Say you filter to `col', only commands
that have the letters `col' inside will be
displayed. To display all commands, press `*' or
`/' and delete the input.

Mouse movement
--------------
You can display the description of a command with
the left mouse button. If you have a wheel mouse,
scrolling is enabled. 

Cross references
----------------
Some commands, such as \\blackrule, inherit
parameters from other commands. Etexshow will
display this as `\\setupblackrules (1)'. You can now
either jump to the command \\setupblackrule or show
the description of \\setupblackrule just below the
currently displayed \\blackrule.

To jump to the command \\setupblackrule, you can
either press the key `J' followed by the number in
the parenthesis or click the middle mouse button on
\\setupblackrule. 

To display the description of \\setuplackrule in
the same buffer, press the number key 1 or click on
the command with the left mouse button. Doing this
again will remove the description.

Comments
--------
You can add comments to the commands. Comments are
saved in a separate file, configured in the variable
`etexshow-comment-file'. If it does not exist when
adding the first comment, it will be automatically
created. Etexshow will record which language the
comment is in. There is a `comment language', which
is shown in the mode line of the description
buffer. You can toggle the comment language with
`L'. To add a comment in the current language,
press `C' in the commands buffer. This will insert
a comment separator (such as \"----comment (en)----\")
and places the cursor behind this separator. Note
that only the region from the comment separator to
the end of the buffer will be saved in the comment
file. 

Etexshow only chooses comments in languages that are
recorded in the variable `etexshow-language-filter'. 
The current value is displayed in the mode line. If 
it is set to '(\"en\" \"de\"), only comments in
languages \"en\" and \"de\" are displayed.


Startup
-------
When cache files do not exist or are outdated, 
etexshow will parse the xml file and create the cache
files. Please note that this will take a long time but
will be done only once in a while. 

Once the cache files exist and are valid, etexshow
will only read them when executed the first time in
a session or when etexshow is run with a prefix
argument, such as `C-u M-x etexshow'. You need to do
this when you make changes to the comments-file
without etexshow or in another session.

About
-----
Etexshow is a port of the perl/tk program texshow to
emacs. The author is Patrick Gundlach. You can find
the latest version at http://levana.de/emacs/ Please
notice that this program is distributed under the
terms of the GNU General Public License as published
by the Free Software Foundation; version 2. If you
have any kind of suggestion or comment please do
not hesitate to send me an email. The address is
given at the top of the help page.
")

  
  (goto-char (point-min))
  
  (if (featurep 'xemacs) ;; arrrrggghhhhh :(
      (view-minor-mode etexshow-cmd-buffer  
		       '(lambda (buf) 
			  (kill-buffer buf)
			  (pop-to-buffer etexshow-cmd-buffer)))
    (view-mode-enter 
     nil 
     '(lambda (visited-buffer) 
	(kill-buffer visited-buffer)
	(pop-to-buffer etexshow-cmd-buffer)))))


;; called only once. Put all comments found in the file into the hash.
(defun etexshow-prepare-comment-buffer (&rest lang)
  "Get buffer with comments in languages LANG. Ignored for now."
  ;; do this crazy stuff only when user has a comment file
  (message "prepare comment-buffer")
  (when (file-readable-p etexshow-comment-file)
    (set-buffer (setq etexshow-comment-buffer 
		      ;; f******ing xemacs! Is there any utf-8 support?
		      (let ((coding-system-for-read 'utf-8))
			(find-file-noselect etexshow-comment-file))))
    (goto-char (point-min))
    (let ((attributeend)
	  (markbegin)
	  (markend)
	  (alist)
	  (hashname))
      ;; that is the real start. Don't use an xml parser, because I need
      ;; to know the positions of the start and endtags
      (re-search-forward "<comments>\n")
      (while (re-search-forward 
	      "<comment " (point-max) t)
	;; ok, found comment now for attributes: eg command="\about"
	(save-excursion 
	  (setq attributeend
		(re-search-forward ">")))
	;; "\\(\\w+\\)=\"\\([^\"]*\\)\""    1=command, 2=\about


	(setq alist nil)
	;; get only attributes from current element
	(while 
	    (re-search-forward "\\(\\w+\\)=\"\\([^\"]*\\)\""
			       attributeend t)
	  (setq alist 
		(append `((,(match-string 1) . ,(match-string 2))) alist)))

	(goto-char attributeend)
	(setq markbegin (make-marker))
	(setq markend (make-marker))
	(set-marker markbegin attributeend)
	(re-search-forward "</comment>")
      
	(set-marker markend (match-beginning 0))
	(set-marker-insertion-type markbegin nil)
	(set-marker-insertion-type markend t)
	(unless (cdr (assoc "language" alist))
	  (message "warning: %s has no language attribute, assuming \"en\""
		   (cdr (assoc "command" alist))))

	(setq hashname (concat (cdr (assoc "command" alist)) 
			       "-"
			       (if (cdr (assoc "language" alist))
				   (cdr (assoc "language" alist))
				 "en")))
	(puthash (etexshow-get-user-visible-command-name 
		  hashname)
		 (list markbegin markend)
		 etexshow-comments-hasht)))))
  
  
(defun etexshow-prepare-command-buffer ()
  "Show command buffer, setup keybindings etc."
  (pop-to-buffer
   (setq etexshow-cmd-buffer
	 (set-buffer (get-buffer-create etexshow-cmd-buffer-name))))
  (kill-all-local-variables)
  (setq mode-line-format ;; xemacs has no :eval  :-(
	(list 
	 " (%l/"
	 'etexshow-commands-count-string
	 ") filter:"
	 'etexshow-current-filter-string
	 "%-"))
  (setq modeline-format mode-line-format)
	      
  ;; xemacs choked
  ;;  (make-variable-buffer-local 'etexshow-cursor-overlay)
  ;; the following will lead to confusion :)
  ;;(make-variable-buffer-local 'etexshow-current-filter-string)
  (setq etexshow-cursor-overlay (make-overlay 1 1))
  (overlay-put etexshow-cursor-overlay  'face 'etexshow-cursor-face)
  (use-local-map etexshow-cmd-buffer-map))

(defun etexshow-prepare-description-buffer ()
  ;; description buffer
  (pop-to-buffer (setq etexshow-description-buffer
		       (set-buffer (get-buffer-create
				    etexshow-description-buffer-name)))
		 nil)
  (use-local-map etexshow-des-buffer-map)
  (setq mode-line-format 
	(list 
	 " Language filter: "
	 '(:eval (mapconcat 'identity etexshow-language-filter " "))
	 "--current language: "
	 'etexshow-comment-language
	 "%-"
	 ))
  (setq modeline-format mode-line-format)
  (setq etexshow-end-description-marker (make-marker))
  (set-marker etexshow-end-description-marker (point-min)))


(defun etexshow-fill-commands (&optional filter)
  "Clear command buffer and display all possible commands. Optional
FILTER is a string. All displayed commands must match FILTER in order
to be displayed."
  (set-buffer etexshow-cmd-buffer)
  (erase-buffer)
  (setq etexshow-commands-count 0)
  (mapc '(lambda (internalname)
	   ;; if filter active, only the matching cmds
	   (when 
	       (or (not filter)
		   (and filter 
			(string-match 
			 (regexp-quote etexshow-current-filter-string) 
			 (etexshow-get-user-visible-command-name
			  internalname))))
	     (aset etexshow-commands-vec etexshow-commands-count internalname)
	     (insert (etexshow-get-user-visible-command-name
		      internalname) "\n")
	     (setq etexshow-commands-count (1+ etexshow-commands-count))))
	etexshow-all-commands)
  (setq etexshow-commands-count-string (number-to-string
					etexshow-commands-count)))




;;; filtering and searching

(defun etexshow-filter ()
  "Activate filter."
  (interactive)
  (setq etexshow-current-filter-string 
	(read-from-minibuffer "Filter: " etexshow-current-filter-string))
  (etexshow-fill-commands etexshow-current-filter-string)
  (goto-char (point-min))
  (etexshow-show-current-description t))

(defun etexshow-un-filter ()
  "Deactivate filter. Set filter to nil."
  (interactive)
  (unless (equal etexshow-current-filter-string "")
    (setq etexshow-current-filter-string "")
    (etexshow-fill-commands)
    (goto-char (point-min)) ;  better: if the command is still seen,
			    ;  goto this command 
    (etexshow-show-current-description t)))

(defun etexshow-isearch ()
  "Search for the string built by current isearch string. Show 
description if found."
  (interactive)
  (let ((foundpoint nil)
	(thischar (this-command-keys)))
    (if (featurep 'xemacs) ;; arrrrggghhhhh :(
	(setq thischar (char-to-string last-command-char)))
    (save-excursion
      ;;(if etexshow-current-isearch-string 
	  (beginning-of-line)
	;;(goto-char (point-min))
	  ;;)
      (when (re-search-forward 
	     (concat (regexp-quote 
		      (concat etexshow-current-isearch-string thischar)))
	     nil t)
	(setq etexshow-current-isearch-string 
	      (concat etexshow-current-isearch-string thischar))
	(setq foundpoint (point))))
    ;; show cursor
    (when foundpoint 
      (goto-char foundpoint)
      (beginning-of-line)
      (etexshow-show-current-description)))
  (message etexshow-current-isearch-string))


(defun etexshow-isearch-again (&optional reversedir)
  "Search again for the string built by current isearch string. Show 
description if found. If REVERSEDIR is non nil, search backward."
  (interactive)
  (when etexshow-current-isearch-string
    (cond (reversedir
	   (beginning-of-line)
	   (re-search-backward 
	    (regexp-quote etexshow-current-isearch-string)
	    nil t))
	  (t 
	   (end-of-line)
	   (re-search-forward 
	    (regexp-quote etexshow-current-isearch-string)
	    nil t)))
    (beginning-of-line)
    (etexshow-show-current-description))
  (message etexshow-current-isearch-string))

(defun etexshow-delete-isearch-char ()
    "Removes the last character of the current isearch string"
  (interactive)
  (if (> (length etexshow-current-isearch-string) 1)
      (setq etexshow-current-isearch-string 
	    (substring etexshow-current-isearch-string 0 -1))
    (setq etexshow-current-isearch-string nil))
  (message etexshow-current-isearch-string))


;;; comment stuff
(defun etexshow-toggle-comment-language ()
  "Switch to next language in etexshow-language-filter."
  (interactive)
  (let ((pos (etexshow-find-equal etexshow-comment-language 
				  etexshow-language-filter)) 
	(len (length etexshow-language-filter)))
    (setq etexshow-comment-language 
	  (nth (mod (1+ pos) len) etexshow-language-filter)))
  (force-mode-line-update))

(defun etexshow-comments-available-in-language-p (internalname &rest filter)
  "Return t if there are comments available in any of the languages
  in FILTER"
  ;;(message "%s" (car filter))
  (let ((langs (car filter))
	(found nil)) ; :-(
    (while (and langs        ; stop if no more language or
		(not found)) ; found 
      (setq found (gethash (concat (etexshow-get-user-visible-command-name
				    internalname) "-" (car langs))  
			   etexshow-comments-hasht))
      (setq langs (cdr langs)))
    found))
  
(defun etexshow-insert-comment-in-language (internalname lang)
  "Insert into current buffer comment in language LANG. If comment in
  language LANG does not exist, ignore it!"
  (let ((pos (point))
	(markerlist (etexshow-get-comment-in-language 
		     internalname lang)))
    (when markerlist
      ;; (message "found %s" lang)
      (insert "\n----comment (" lang ")----\n")
      (insert-buffer-substring 
       etexshow-comment-buffer 
       (nth 0 markerlist) 
       (nth 1 markerlist))
      (goto-char pos)
      (etexshow-decode-from-here))))
  

(defun etexshow-insert-all-comments (internalname)
  "Insert comments for all languages in language-filter"
  ;;(message "inserting comments for all languages")
  (mapc '(lambda (lang) 
	   (etexshow-insert-comment-in-language internalname lang))
	etexshow-language-filter))


(defun etexshow-get-comment-in-language (internalname lang)
  "Return markerlist for a comment in language LANG or nil if no such \
comment exists."
  ;;(message "looking for %s" (concat (etexshow-get-user-visible-command-name
  ;; internalname) "-" lang))
  (gethash (concat (etexshow-get-user-visible-command-name
				     internalname) "-" lang)
	   etexshow-comments-hasht))

(defun etexshow-comment-command ()
  "Add a comment for this command into description buffer."
  (interactive)
  (let* ((internalname (etexshow-current-internal-name))
	 (markerlist (etexshow-get-comment-in-language 
		      internalname etexshow-comment-language)))
    
    (pop-to-buffer etexshow-description-buffer)
    (erase-buffer)
    (etexshow-display-syntax-internal internalname)
    (etexshow-insert-comment-in-language internalname etexshow-comment-language)    (message "Press C-c C-c when finished editing")
    (if markerlist 
	(progn 
	  (goto-char (point-min))
	  (re-search-forward etexshow-comment-regexp))
      (goto-char (point-max))
      (insert (format etexshow-comment-formatstring 
		      etexshow-comment-language )))))

(defun etexshow-delete-comment (&optional force)
  "Remove the comment from the current command. FORCE is not yet implemented."
  (interactive)
  (let* ((internalname (etexshow-current-internal-name))
	 (markerlist)
	 (hashname)
	 (count 0)
	 (languages)) ;; there are comments in these languages

    ;; how many comments are there?
    (set-buffer etexshow-description-buffer)
    (goto-char (point-min))
    (while (re-search-forward etexshow-comment-regexp (point-max) t)
      (setq languages (append languages (list (match-string 1)))))
    
    ;;(message "found languages %s" languages)
    ;; (message "etexshow-comment-language %s" etexshow-comment-language)
    (if (> (length languages) 1)
	(setq languages (completing-read  
			 (concat "delete comment in language (" 
				 etexshow-comment-language
				 "): ")
			 (mapcar '(lambda (lan)
				    (setq count (1+ count))
				    (list lan count))
				 languages) 
			 nil t nil nil etexshow-comment-language))
      (setq languages (car languages))) ;; only one
    (message "deleting %s" languages)
    (setq hashname (concat (etexshow-get-user-visible-command-name
			    internalname) "-" languages))
    (setq markerlist (gethash hashname
			      etexshow-comments-hasht))
    (when (y-or-n-p "Delete Comment? ")
      (if (not markerlist)
	  (message "No comment to delete")
	(set-buffer etexshow-comment-buffer)
	(goto-char (nth 0 markerlist))
	(delete-region (nth 0 markerlist) (nth 1 markerlist))
	(re-search-backward "<comment")
	(delete-region (point) (re-search-forward "</comment>\n"))
	(remhash hashname etexshow-comments-hasht)
	(save-buffer))
      (pop-to-buffer etexshow-cmd-buffer)
      (etexshow-display-syntax internalname))))


(defun etexshow-create-comment-buffer ()
  "Create an (almost) empty buffer for the comments."
  (find-file-noselect etexshow-comment-file)
  (set-buffer (setq etexshow-comment-buffer 
		    (find-file-noselect etexshow-comment-file)))
  (insert "<?xml version=\"1.0\"?>\n\n<comments>\n</comments>\n")
  (setq save-buffer-coding-system 'utf-8))
  
(defun etexshow-save-comment ()
  "Save the newly entered comment into the comment-file."
  (interactive)
  (goto-char (point-min))
  (let* ((internalname (etexshow-current-internal-name))
	 (markbegin)
	 (markend)
	 (user-visible-name (etexshow-get-user-visible-command-name
				 internalname))
	 (begin (re-search-forward etexshow-comment-regexp))
	 (end (point-max))
	 (language (match-string 1))
	 (markerlist (etexshow-get-comment-in-language 
		      internalname language))
	 (hashname))
    ;; don't use [:xspace:] in regegexps. xemacs doesn't know about this
    (cond ((not (re-search-forward "[^	 \n]" (point-max) t)) 
	   ;; no comment to save
	   (message "No comment to save")
	  
	   ;; perhaps we have to delete the comment from the buffer?
	   (when markerlist 
	     (etexshow-delete-comment)))
	  
	  (t ;; there is something to save!
	   (unless (buffer-live-p etexshow-comment-buffer)
	     (etexshow-create-comment-buffer))
	   (set-buffer etexshow-comment-buffer)
	   (if markerlist
	       (progn
		 (delete-region (nth 0 markerlist) (nth 1 markerlist))
		 (goto-char (nth 0 markerlist)))

	     ;; not been in the comment-file before
	     (goto-char (point-max))
	     (re-search-backward "</comments>")
	     (insert "<comment command=\""
		     (substring user-visible-name 1)
		     "\" language=\"" language "\""
		     "\>")
	     (setq markbegin (make-marker))
	     (setq markend (make-marker))
	     (set-marker markbegin (point))

	     ;; XXX perhaps insert-before-marker or sth. like this?
	     (save-excursion
	       (insert "</comment>\n"))
	     
	     (set-marker markend (point))
	     (set-marker-insertion-type markbegin nil)
	     (set-marker-insertion-type markend t)
	     (setq hashname (concat user-visible-name "-" language))
	     (message "saving %s" hashname)
	     (puthash hashname 
		      (list markbegin markend)
		      etexshow-comments-hasht))
	   (etexshow-insert-buffer-substring etexshow-description-buffer begin
					     end)
	   (save-buffer))))
  (pop-to-buffer etexshow-cmd-buffer))

(defun etexshow-insert-buffer-substring (buf start end)
  "Same as insert-buffer-substring but encode dangerous (xml) chars."
  (insert 
   (with-temp-buffer
     (insert-buffer-substring etexshow-description-buffer start end)
     (goto-char (point-min))
     (etexshow-encode-from-here)
     (buffer-string))))
  
(defun etexshow-encode-from-here ()
  "Encode bad characters < and > for saving in xml file." 
  (let ((pos (point)))
    ;; TODO: generalize this!
    (while (search-forward "<" (point-max) t)
      (replace-match "&lt;"))
    (goto-char pos)
    (while (search-forward ">"  (point-max) t)
      (replace-match "&gt;"))))

(defun etexshow-decode-from-here ()
  "Decode bad characters < and > for displaying in description buffer." 
  ;; TODO: generalize this!
  (let ((pos (point)))
    (while (search-forward "&lt;" (point-max) t)
      (replace-match "<"))
    (goto-char pos)
    (while (search-forward "&gt;" (point-max) t)
      (replace-match ">"))))


;;; filling the description buffer

(defun etexshow-display-syntax (internalcommand)
  "Show in description buffer description syntax of INTERNALCOMMAND."
  (set-buffer etexshow-description-buffer)
  (erase-buffer)
  (setq etexshow-hyperlink-alist nil)
  (setq etexshow-hyperlink-marker-alist nil)
  (setq etexshow-hyperlink-alist-alist nil)
  (etexshow-display-syntax-internal internalcommand)
  (etexshow-insert-all-comments internalcommand))
   ;;  (etexshow-insert-comment internalcommand))


(defun etexshow-display-syntax-internal (internalcommand)
  ;;(message "inserting %s" internalcommand)
  (insert "\n")
  (let* ((node (gethash internalcommand etexshow-commands-hasht))
	 (environment-p (string= (pgxml-get-attribute node 'type)
				 "environment")))
    ;; (print (pgxml-get-attribute node 'type) 'insert)
    (etexshow-insert-sequence (pgxml-get-children node 'sequence)
			      environment-p)
    (etexshow-insert-arguments (pgxml-get-children node 'arguments))
    (when environment-p
      (etexshow-insert-stopsequence 
       (pgxml-get-children node 'sequence)))

    (insert "\n\n\n")
    ;; now for the explanation
    (etexshow-insert-explanation (pgxml-get-children node
						     'arguments)))
  ;; this needs to stay here, because the hyperlinked commands shift
  ;; this towards the end
  (set-marker etexshow-end-description-marker (point)))

(defun etexshow-toggle-inherit-mouse ()
  (interactive)
  (set-buffer etexshow-description-buffer) 
  (mouse-set-point last-nonmenu-event)
  (save-excursion
    (re-search-backward " " (point-min) t)
    (when (looking-at " \\\\\\w+ (\\([0-9]+\\))")
      (etexshow-toggle-inherit-reference 
       (string-to-number (match-string 1)))))
  (pop-to-buffer etexshow-cmd-buffer)) 


(defun etexshow-toggle-inherit-reference (&optional nth)
  "Insert into description buffer NTH (if invoked with argument)
reference. The reference must be given in the symbol
`etexshow-hyperlink-alist'. This funktion should be invoked with
  argument NTH (integer 1-9) or issued with the key 1-9."
  (interactive)
  (unless nth
    (setq nth (string-to-number (if (featurep 'xemacs)
				    (char-to-string last-command-char)
				  (this-command-keys)))))
  (when (assoc nth etexshow-hyperlink-alist)
    (if (assoc nth etexshow-hyperlink-marker-alist) ;being displayed
	(progn 
	  (let* ((markeralist (assoc nth etexshow-hyperlink-marker-alist))
		 (markerlist (car (cdr markeralist))))
	    ;;(message "removing")
	    ;; ok, remove the shown stuff
	    (set-buffer etexshow-description-buffer)
	    (delete-region (car markerlist)
			   etexshow-end-description-marker))
	  (setq etexshow-hyperlink-marker-alist
		(car (car (cdr 
			   (assoc nth etexshow-hyperlink-alist-alist)))))
	  (setq etexshow-hyperlink-alist 
		(car (cdr (car (cdr 
				(assoc nth etexshow-hyperlink-alist-alist))))))
	  (setq etexshow-hyperlink-alist-alist
		(assq-delete-all nth etexshow-hyperlink-alist-alist))
	  (setq etexshow-hyperlink-marker-alist
		(assq-delete-all nth
				 etexshow-hyperlink-marker-alist)))
      
      (let ((markbegin (make-marker))
	    (markend (make-marker))
	    (hyperlink-alist))
	;;(message "inserting inherit for %s hyperlink" nth)
	(set-buffer etexshow-description-buffer)
	(goto-char etexshow-end-description-marker)
	(set-marker markbegin (point))
	;; etexshow-hyperlink-alist 
	(setq hyperlink-alist (copy-alist etexshow-hyperlink-alist))
	(etexshow-display-syntax-internal 
	 (cdr (assoc nth etexshow-hyperlink-alist)))
	(set-marker markend (point))
	(setq etexshow-hyperlink-marker-alist 
	      (append `((,nth (,markbegin ,markend)))
		      etexshow-hyperlink-marker-alist))
	(setq etexshow-hyperlink-alist-alist 
	      (append 
	       `((,nth (,etexshow-hyperlink-marker-alist 
			,hyperlink-alist)))
	       etexshow-hyperlink-alist-alist ))))))

  
(defun etexshow-insert-sequence (xmlparsedseqence environment-p)
  "Insert into current buffer the name of the command.  This is the
command in the xml node presented by xmlparsedsequence and it will be
displayed so that the user recognize the real name. For example
\\startfloattext is not a command name but the part float should be
replaced by a real float"
  
  (insert (if environment-p "\\start" "\\")); env begin with \start...
  (let ((beg)
	(overlay))
    (mapcar '(lambda (sequenceelement)
	       (cond ((eq (pgxml-node-name sequenceelement) 'string)
		      (insert (pgxml-get-attribute sequenceelement 'value )))
		     ((eq (pgxml-node-name sequenceelement)
			  ;;'seqvar)
			  'variable)
		      (setq beg (point))
		      (insert (pgxml-get-attribute sequenceelement 'value))
		      (setq overlay (make-overlay beg (point)))
		      (overlay-put overlay 'face etexshow-variable))
		     (t (message "unknown sequence element"))))
	    (pgxml-node-children (car xmlparsedseqence)))))

(defun etexshow-insert-stopsequence (xmlparsedseqence)
  "Insert into current buffer the \\stop...name of the command. " 
  (let ((beg)
	(overlay))
    (insert " ... \\stop" )

    (mapcar '(lambda (sequenceelement)
	       (cond ((eq (pgxml-node-name sequenceelement) 'string)
		      (insert (pgxml-get-attribute sequenceelement 'value )))
		     ((eq (pgxml-node-name sequenceelement)
			  ;;'seqvar)
			  'variable)
		      (setq beg (point))
		      (insert (pgxml-get-attribute sequenceelement 'value))
		      (setq overlay (make-overlay beg (point)))
		      (overlay-put overlay 'face etexshow-variable))
		     (t (message "unknown sequence element"))))
	    (pgxml-node-children (car xmlparsedseqence)))))



(defun etexshow-insert-arguments (xmlparsedarguments &optional choice)
  "Insert into current buffer the arguments of the command. The
command is represented by the xmlparsedarguments"
;;  (myprint xmlparsedarguments)
  (let ((beg)
	(overlay)
	(count 0))
    (mapcar `(lambda (elt)
	       (when (eq (pgxml-node-name elt) 'choice)
		 (etexshow-insert-arguments (list elt) t))
	       (setq beg (point))
	       (etexshow-withface-count 
		count
		(cond ((eq (pgxml-node-name elt) 'choice)
		       (ignore))
		      ((eq (pgxml-node-name elt) 'keywords)
		       (etexshow-insert-argument-keywords elt t))
		      ((eq (pgxml-node-name elt) 'tex)
		       (etexshow-insert-argument-tex elt t))
		      ((eq (pgxml-node-name elt) 'position)
		       (etexshow-insert-argument-position elt t))
		      ((eq (pgxml-node-name elt) 'index)
		       (etexshow-insert-argument-index elt t))
		      ((eq (pgxml-node-name elt) 'file)
		       (etexshow-insert-argument-file elt t))
		      ((eq (pgxml-node-name elt) 'displaymath)
		       (etexshow-insert-argument-displaymath elt t))
		      ((eq (pgxml-node-name elt) 'nothing)
		       (etexshow-insert-argument-nothing elt t))
		      ((eq (pgxml-node-name elt) 'word)
		       (etexshow-insert-argument-word elt t))
		      ((eq (pgxml-node-name elt) 'reference)
		       (etexshow-insert-argument-reference elt t))
		      ((eq (pgxml-node-name elt) 'content)
		       (etexshow-insert-argument-content elt t))
		      ((eq (pgxml-node-name elt) 'assignments)
		       (etexshow-insert-argument-assignments elt t))
		      ((eq (pgxml-node-name elt) 'triplet)
		       (etexshow-insert-argument-triplet elt t))
		      ((eq (pgxml-node-name elt) 'csname)
		       (etexshow-insert-argument-csname elt t))
		      (t (print elt 'insert))))
	       
		(when (string= (pgxml-get-attribute elt 'optional) "yes")
		  (setq overlay (make-overlay beg (point)))
		  (overlay-put overlay 'face etexshow-optional))
		(when choice
		  (insert "|"))
		(setq count (1+ count)))
	    (pgxml-node-children (car xmlparsedarguments)))
    (when choice (backward-delete-char-untabify 1))))


(defun etexshow-insert-explanation (xmlparsedarguments &optional choice)
  "Insert into current buffer the arguments of the command. The
command is represented by the xmlparsedarguments"
  (let ((count 0))
    (mapcar '(lambda (elt)
	       (when (eq (pgxml-node-name elt) 'choice)
		 (etexshow-insert-explanation (list elt) t))
	       (etexshow-withface-count 
		count
		(cond ((eq (pgxml-node-name elt) 'choice)
		       (ignore))
		      ((eq (pgxml-node-name elt) 'keywords)
		       (etexshow-insert-argument-keywords elt nil))
		      ((eq (pgxml-node-name elt) 'tex)
		       (etexshow-insert-argument-tex elt nil))
		      ((eq (pgxml-node-name elt) 'position)
		       (etexshow-insert-argument-position elt nil))
		      ((eq (pgxml-node-name elt) 'index)
		       (etexshow-insert-argument-index elt nil))
		      ((eq (pgxml-node-name elt) 'file)
		       (etexshow-insert-argument-file elt nil))
		      ((eq (pgxml-node-name elt) 'displaymath)
		       (etexshow-insert-argument-displaymath elt nil))
		      ((eq (pgxml-node-name elt) 'nothing)
		       (etexshow-insert-argument-nothing elt nil))
		      ((eq (pgxml-node-name elt) 'word)
		       (etexshow-insert-argument-word elt nil))
		      ((eq (pgxml-node-name elt) 'reference)
		       (etexshow-insert-argument-reference elt nil))
		      ((eq (pgxml-node-name elt) 'triplet)
		       (etexshow-insert-argument-triplet elt nil))
		      ((eq (pgxml-node-name elt) 'csname)
		       (etexshow-insert-argument-csname elt nil))
		      ((eq (pgxml-node-name elt) 'content)
		       (etexshow-insert-argument-content elt nil))
		      ((eq (pgxml-node-name elt) 'assignments)
		       (etexshow-insert-argument-assignments elt nil))
		      (t (print elt 'insert))))
	       (setq count (1+ count)))
	    (pgxml-node-children (car xmlparsedarguments)))))

(defun etexshow-insert-argument-tex (tex symbol-only)
  "Insert into current buffer the placeholder for TEX . When symbol-only is
not nil, insert symbol and description."
  (when symbol-only
    (when (string= (pgxml-get-attribute tex 'separator) "backslash")
      (insert "\\\\"))
    (when (pgxml-get-attribute tex 'csname)
      (insert "\\" (pgxml-get-attribute tex 'command)))))

(defun etexshow-insert-argument-position (pos symbol-only)
  "Insert into current buffer the position POS. When symbol-only is
not nil, insert symbol and description."
  (insert (if (string= (pgxml-get-attribute pos 'list) "yes")
	      "(...,...)" ;; only seen with list=yes
	    "(...)")) ;; this looks very strange
  (unless symbol-only
    (indent-to-column etexshow-preferred-column etexshow-min-dist)
    (insert 
     (if (string= (pgxml-get-attribute pos 'list) "yes")
	 "<number>, <number>" ;; don't hardcode this XXX
       "<number>")
     "\n")))

(defun etexshow-insert-argument-index (ind symbol-only)
  "Insert into current buffer the index IND. When symbol-only is
not nil, insert symbol and description."
  (insert (if (string= (pgxml-get-attribute ind 'list) "yes")
	      "{..+..+..}"  ;; is this correct? XXX
	    "{...}")) ;; only seen with list=no.  why?
  (unless symbol-only
    (insert "\n")))

(defun etexshow-insert-argument-file (file symbol-only)
  "Insert into current buffer the representation of file FILE. When
SYMBOL-ONLY is not nil, insert symbol and description."
  (insert "...")
  (unless symbol-only 
    (indent-to-column etexshow-preferred-column etexshow-min-dist)
    (insert "<file>\n")))

(defun etexshow-insert-argument-displaymath (dsp symbol-only)
  "Insert into current buffer the representation of displaymath DSP. When
SYMBOL-ONLY is not nil, insert symbol and description."
  (insert "$$...$$")
  (unless symbol-only 
    (indent-to-column etexshow-preferred-column etexshow-min-dist)
    (insert "<formula>\n")))

(defun etexshow-insert-argument-nothing (what symbol-only)
  "Insert into current buffer the placeholder for WHAT . When symbol-only is
not nil, insert symbol and description."
  (if symbol-only
    (when (string= (pgxml-get-attribute what 'separator) "backslash")
      (insert "\\\\")))
  (insert "...")
  (unless symbol-only
    (indent-to-column etexshow-preferred-column etexshow-min-dist)
    (insert "<text>\n")))

(defun etexshow-insert-argument-word (word symbol-only)
  "Insert into current buffer the reference REF. When symbol-only is
not nil, insert symbol and description."
  (insert (if (string= (pgxml-get-attribute word 'list) "yes")
	      "{... ...}"
	    "{...}")) ;; only seen with list=yes. not true: Word
  (unless symbol-only
    (insert "\n")))

(defun etexshow-insert-argument-reference (ref symbol-only)
  "Insert into current buffer the reference REF. When symbol-only is
not nil, insert symbol and description."
  (insert (if (string= (pgxml-get-attribute ref 'list) "yes")
	      "[ref,ref,...]"
	    "[ref]"))
  (unless symbol-only
    (insert "\n")))


(defun etexshow-insert-argument-triplet (trip symbol-only)
  "Insert into current buffer the triplet TRIP. When SYMBOL-ONLY is
not nil, insert symbol and description."
  (insert (if (string= (pgxml-get-attribute trip 'list) "yes")
	      "[x:y:z=,...]"
	    "[x:y:z]"))  ;;never seen in cont-en.xml!!!
  (unless symbol-only
    (insert "\n")))


(defun etexshow-insert-argument-csname (csname symbol-only)
  "Insert into current buffer the control sequence CSNAME. When
SYMBOL-ONLY is not nil, insert symbol and description."
  (when symbol-only 
    (insert (if (string= (pgxml-get-attribute csname 'list) "yes")
		"{... ...}" ;; never seen list=yes
	      "\\command"))))


(defun etexshow-translate (arg)
  "Translate ARG (\"cd:name\") to something like \"<name>\"."
  (let ((pos (string-match "cd:.*$" arg)))
  (if pos 
      (concat (substring arg 0 pos)
	      "<" (substring arg  (+ 3 pos))  ">")
    arg)))

(defun etexshow-insert-constant (elt)
  (let ((beg)
	(overlay))
    (setq beg (point))
    (insert (etexshow-translate (pgxml-get-attribute elt 'type)))
    ;; underline default
    (when (string= (pgxml-get-attribute elt 'default) "yes")
      (overlay-put (make-overlay beg (point)) 
		   'face etexshow-default))))

(defun etexshow-insert-variable (var)
  (let ((beg)
	(overlay))
    (setq beg (point))
    (insert (etexshow-translate (pgxml-get-attribute var 'type)))
    ;; underline default 
    (when (string= (pgxml-get-attribute var 'default) "yes")
      (overlay-put (make-overlay beg (point)) 
		   'face etexshow-default))))


(defun etexshow-insert-inherit (elt)
  (let ((count (1+ (length etexshow-hyperlink-alist)))
	(beg)
	(end))
    (setq etexshow-hyperlink-alist 
	  (append `((,count .
			    ,(pgxml-get-attribute elt 'name)))
		  etexshow-hyperlink-alist))
    (insert 
     "see ")
    (setq beg (point))
    (insert "\\" 
     (pgxml-get-attribute elt 'name))
    (setq end (point))
    (insert 
     " (" (number-to-string count) ")")
    (add-text-properties 
     beg end
     '(font-lock-face info-xref
		      mouse-face highlight
		      help-echo "mouse-1: toggle cmd, m-2: jump to cmd"))))

(defun etexshow-break-if-necessary ()
  "Insert linebreak if necessary in description buffer."
  (cond ((>= (current-column) 
	    (window-width (get-buffer-window etexshow-description-buffer t)))

	 ;; jump to the beginning of the previous word, delete last
	 ;; space, insert newline and then indent that word and jump
	 ;; to the end (ugly)
         (skip-chars-backward "^ ")
	 (delete-char -1)
         (insert "\n")
         (indent-to-column etexshow-preferred-column etexshow-min-dist)
	 (goto-char (point-max)))))


(defun etexshow-insert-argument-keywords (keyw symbol-only)
  "Insert into current buffer the keyword KEYW. When symbol-only is
not nil, insert symbol and description as found in the children of the
keyword (constant, inherit, variable)."
  (insert (if (string= (pgxml-get-attribute keyw 'list) "yes")
	      "[...,...,...]"
	    "[...]"))
  (unless symbol-only
    (indent-to-column etexshow-preferred-column etexshow-min-dist)
    (backward-delete-char-untabify 1) ; just goin' back would "add" a
				      ; space -> evil
    (mapcar '(lambda (elt)
	       (insert " ")
	       (cond ((eq (pgxml-node-name elt) 'constant)
		      (etexshow-insert-constant elt))
		     ((eq (pgxml-node-name elt) 'variable)
		      (etexshow-insert-variable elt))
		     ((eq (pgxml-node-name elt) 'inherit)
		      (etexshow-insert-inherit elt))
		     (t (print elt 'insert))) ;; what is missing?
	       (etexshow-break-if-necessary))
	    (pgxml-node-children keyw))
    (insert "\n")))

(defun etexshow-insert-assignment-parameter (para)
  "Insert into current buffer the parameters PARA."
  (insert (etexshow-translate (pgxml-get-attribute para 'name)))
  (indent-to-column etexshow-preferred-column etexshow-min-dist)
  (backward-delete-char-untabify 1)
  (mapcar '(lambda (elt)
	     (insert " ")
	     (cond ((eq (pgxml-node-name elt) 'constant)
		    (etexshow-insert-constant elt))
		   ((eq (pgxml-node-name elt) 'variable)
		    (etexshow-insert-variable elt))
		   (t (print elt 'insert)))
	     (etexshow-break-if-necessary)
	     ) ;; something missing?
	  (pgxml-node-children para)))


(defun etexshow-insert-argument-content (cont symbol-only)
  "Insert into current buffer the content CONT . When symbol-only is
not nil, insert symbol and description."
  (insert (if (string= (pgxml-get-attribute cont 'list) "yes")
	      "{...,...,...}"
	    "{...}"))
  
  (unless symbol-only
    (indent-to-column etexshow-preferred-column etexshow-min-dist)
    (insert "<text>")
    (insert "\n")))

(defun etexshow-insert-argument-assignments (assg symbol-only)
  "Insert into current buffer the assignments ASSG. When symbol-only is
not nil, insert symbol and description as found in the children of the
assignment (constant, inherit, parameter)."
  (let ((insertstring (if (string= (pgxml-get-attribute assg 'list) "yes")
			  "[...,...=...,...]"
			"[...=...]")))
    (if symbol-only
	(insert insertstring)
      (mapcar '(lambda (elt)
		 ;;	       (insert " ")
		 (cond ((eq (pgxml-node-name elt) 'parameter)
			(etexshow-insert-assignment-parameter elt))
		       ((eq (pgxml-node-name elt) 'inherit)
			(insert insertstring)
			(indent-to-column etexshow-preferred-column 
					  etexshow-min-dist)
			(etexshow-insert-inherit elt))
		       (t (print elt 'insert)))
		 (insert "\n")) ;; constant missing
	      (pgxml-node-children assg)))))



;;; Movement and alike

(defun etexshow-show-mouse ()
  "Set point and cursor to position where mouse click was."
  (interactive)
  (mouse-set-point last-nonmenu-event)
  (etexshow-show-current-description t))
  
(defun etexshow-scroll-down-some-files (&optional dir)
  "Set cursor some commands closer to the beginning/end. DIR is the
direction, negative means cursor towards the first command, postive
or nil means cursor closer to the end." 
  (interactive) 
  (etexshow-delete-isearch-char)
  (let ((mult  (cond ((not dir) 1)
		     ((>= dir 0) 1)
		     (t -1)))) ; mult is positive if cursor goes down  
    (etexshow-goto-command
     (+ 1
	(etexshow-commandnumber-at-point) ; starts with 0
	(* (- (window-height) 1) mult)))))

(defun etexshow-commandnumber-at-point ()
  "Return the line number (=command number) at point."
  (save-excursion
    (beginning-of-line)
    (count-lines (point-min) (point))))


(defun etexshow-next-line (&optional arg)
  "Move cursor down (up when ARG is not nil) one line."
  (interactive)
  (etexshow-goto-command 
   (+ (etexshow-commandnumber-at-point) (if arg 0 2))))

(defun etexshow-show-current-description (&optional del )
  "Show the description of the command the cursor is on. If \
DEL is not nil, delete the current isearch-string." 
  (when del
      (setq etexshow-current-isearch-string nil))
  (save-excursion
    (beginning-of-line)
    (move-overlay etexshow-cursor-overlay
		  (point)
		  (progn 
		    (forward-line)
		    (beginning-of-line)
		    (point))))
  (etexshow-display-syntax
   (aref etexshow-commands-vec
	 (save-excursion
	   (beginning-of-line)
	   (count-lines (point-min) (point)))))
  (goto-char (point-min))
  (set-buffer etexshow-cmd-buffer))
				   

(defun etexshow-goto-reference-mouse ()
  "Follow hyperlink to command."
  (interactive)
  (let ((cmd))
    (set-buffer etexshow-description-buffer) 
    (mouse-set-point last-nonmenu-event)
    (re-search-backward " " (point-min) t)
    (when (looking-at " \\\\\\(\\w+\\)")
      (setq cmd (match-string-no-properties 1))
      (setq etexshow-command-history 
	    (append (list (substring 
			   (etexshow-get-user-visible-command-name
			    (etexshow-current-internal-name))
			   1))
		    etexshow-command-history))
      (etexshow-goto-command cmd))))

(defun etexshow-goto-reference (nth)
  "Ask user for reference number and goto this command"
  (interactive "cGoto reference: (1-9)")
  (setq nth (string-to-number (char-to-string nth)))
  (when (and  (numberp nth)
	      (assoc nth etexshow-hyperlink-alist))
    (setq etexshow-command-history 
	  (append (list (substring (etexshow-get-user-visible-command-name
				    (etexshow-current-internal-name))
				   1))
		  etexshow-command-history))
    (etexshow-goto-command (cdr (assoc nth etexshow-hyperlink-alist)))))
  
(defun etexshow-goto-command (cmd)
  "Goto command CMD. CMD can be either a line number or a 
command name (string)."
  (cond ((numberp cmd)
	 (goto-line (if (< cmd etexshow-commands-count)
			cmd
		      etexshow-commands-count)))
	((stringp cmd)
	 (etexshow-un-filter)
	 (pop-to-buffer etexshow-cmd-buffer)
	 (goto-char (point-min))
	 (re-search-forward (concat "\\\\" cmd))
	 (beginning-of-line))
	(t (message "etexshow-goto-command: unknown parameter type")))
  (etexshow-show-current-description t))
  
(defun etexshow-back-history ()
  "Jump one command back in history if this is possible"
  (interactive)
  (let ((cmd (car etexshow-command-history)))
    (setq etexshow-command-history (cdr etexshow-command-history))
    (when cmd 
      (etexshow-goto-command cmd))))


;;; init

(defun etexshow-init ()
  "prepare necessary datastructure -- merge with etexshow-prepare-datastru.."
  ;; all the xml and cache files
  (mapcar '(lambda (arg) 
	     (unless (file-readable-p (car arg)) ;;xml-file
	       (error  "cannot read file %s, exiting" (car arg)))
	     (unless (file-writable-p (cdr arg)) ;; cache file
	       (error  "cannot write file %s, exiting" (cdr arg))))
	  etexshow-xml-files-alist)


  (setq etexshow-current-filter-string "")
  (if (and (buffer-live-p etexshow-cmd-buffer)
	   (buffer-live-p etexshow-description-buffer))
      ;; not needed: remove XXX
      (set-window-configuration etexshow-window-configuration)
    ;; not both buffers alive
    (setq etexshow-before-window-configuration (current-window-configuration))
    (etexshow-prepare-command-buffer)
    (split-window nil 25 t)
    (other-window -1)
    (etexshow-prepare-description-buffer)
    (setq etexshow-window-configuration (current-window-configuration))))




;;; fill datastructure
(defun etexshow-prepare-datastructures ()
  "Init the hash table and the commands list"
  
  (message "Preparing datatstructure...")
  (when (hash-table-p etexshow-commands-hasht)
    (clrhash etexshow-commands-hasht)
    (clrhash etexshow-comments-hasht))
  
  (setq etexshow-commands-hasht
	(make-hash-table :test 'equal :size 600)) ;; :test 'etexshow-hashtest

  (setq etexshow-comments-hasht
	(make-hash-table :test 'equal :size 600)) ;; :test 'etexshow-hashtest

    
  (mapcar '(lambda (arg) ;; arg = ("xmlfile" . "cachefile")
	     (let* ((xml-filename (car arg))
		    (xml-cache-file (cdr arg))
		    (cache-buf))
	       (save-excursion
		 (setq cache-buf (find-file-noselect xml-cache-file t))
		 ;; (kill-all-local-variables)
		 (if (not (file-newer-than-file-p xml-filename xml-cache-file))
		     (progn 
		       (set-buffer cache-buf)
		       (message "Reading from cache...")
		       (goto-char (point-min))
		       (setq etexshow-xml-top-node
			     (read cache-buf))
		       (message "Reading from cache...done"))
		   (message "Parsing xml-file...")
		   (save-excursion 
		     (setq etexshow-xml-top-node
			   (car (pgxml-parse-file xml-filename))))
		   (message "Parsing xml-file...done. Now generating cache (be patient).")
		   (set-buffer cache-buf)
		   ;; this is so slow. I don't know how to speed this up
		   (kill-all-local-variables)
		   (setq buffer-undo-list t)
		   (erase-buffer)
		   (print etexshow-xml-top-node 'insert)
		   (write-file xml-cache-file)
		   (message "generating cache...done")))
	       (etexshow-fill-hasht etexshow-xml-top-node)
	       (kill-buffer cache-buf)))
	  etexshow-xml-files-alist)
    
  (setq etexshow-commands-vec
	(make-vector (hash-table-count etexshow-commands-hasht) nil))
  
  (setq etexshow-all-commands nil)
  (maphash '(lambda (first second)
	      (setq  etexshow-all-commands
		     (append (list first )
			     etexshow-all-commands)))
	   etexshow-commands-hasht)
  ;;  'etexshow-all-commands' looks like this:
  ;;  ("definepalet" "color" "*color" "definecolor" "setupcolor" ...)

  ;; sort only once!
  (setq etexshow-all-commands
	(sort etexshow-all-commands
	      '(lambda (first second)
		 (string<
		  (downcase 
		   (etexshow-get-user-visible-command-name first))
		  (downcase 
		   (etexshow-get-user-visible-command-name second))))))
  
  t)
  


(defun etexshow-get-hashname (command)
  "Find the name to be used for the hashentry."
  ;; there is a command \color and an environment \startcolor...\stopcolor
  ;; so you cannot simply use the name attribute of command, which
  ;; both are color.
  (concat
   (when (string= (pgxml-get-attribute command 'type) "environment")
     "*")
   (pgxml-get-attribute command 'name)))


(defun etexshow-fill-hasht (parent)
  (mapcar '(lambda (command)
	     (puthash (etexshow-get-hashname command)
		      command
		      etexshow-commands-hasht))
	  (pgxml-node-children parent))) ; all command


(defun etexshow-get-user-visible-command-name (internalname)
  "Get the user visible command name."

  ;; environments are marked like this: *color = \startcolor ... \stopcolor
  (concat "\\"
	  (if (string= "*" (string (elt internalname 0)))
	      (concat "start" (substring internalname 1))
	    internalname)))


(provide 'etexshow)

;;; etexshow.el ends here
