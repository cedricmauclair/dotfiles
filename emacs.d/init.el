;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (c) 2011, Cédric Mauclair.
;;
;; Licenced under GPL v3.0.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;<< -- server                 ------------------------------------- >>

(load "server" t t)

(when (and (functionp 'server-running-p)
   (not (server-running-p)))
  (server-start))

;>> server (end) -------------------------------------------------- >>
;<< -- load-path              ------------------------------------- >>

(message default-directory)

(defconst emacs-root
  (expand-file-name (concat "~" init-file-user "/.emacs.d/"))
  "The root directory of your personnal emacs tree.")

(defconst lisp-root
  (concat emacs-root "lisp/")
  "Root directory of your lisp tree.")

(defconst themes-root
  (concat emacs-root "themes/")
  "Root directory of your themes tree.")

(add-to-list 'load-path lisp-root)
(add-to-list 'custom-theme-load-path themes-root)

(defconst more-lisp-dirs
  (list "auctex" "etexshow")
  "List of directories to add to the load path, relative to
    `lisp-root' or absolute.")

(dolist (dir more-lisp-dirs)
  (add-to-list 'load-path
               (if (file-name-absolute-p dir) dir
                 (concat lisp-root dir))))

;>> load-path (end) ----------------------------------------------- >>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;<< -- before-save-hook & write-file-hooks ------------------------ >>

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'write-file-hooks 'time-stamp)

;>> before-save-hook & write-file-hooks (end) --------------------- >>
;<< -- enable/disable some commands        ------------------------ >>

(put 'dired-find-alternate-file 'disabled nil)

;>> enable/disable some commands (end) ---------------------------- >>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;<< -- custom-set-variable    ------------------------------------- >>

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ConTeXt-font-list (quote ((2 "{\\bf " "}") (3 "{\\sc " "}") (5 "{\\em " "}") (9 "{\\it " "}") (18 "{\\rm " "}") (19 "{\\sl " "}") (20 "{\\tt " "}") (4 "" "" t) (8 "\\high{" "}") (12 "\\low{" "}") (13 "\\hilo{" "}{}") (6 "{\\english " "}"))))
 '(TeX-auto-save t)
 '(TeX-command-list (quote (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "context --once %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "context %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-electric-sub-and-superscript t)
 '(TeX-expand-list (quote (("%p" TeX-printer-query) ("%q" (lambda nil (TeX-printer-query t))) ("%V" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-view-command-raw))) ("%vv" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-output-style-check TeX-output-view-style))) ("%v" (lambda nil (TeX-source-correlate-start-server-maybe) (TeX-style-check TeX-view-style))) ("%r" (lambda nil (TeX-style-check TeX-print-style))) ("%l" (lambda nil (TeX-style-check LaTeX-command-style))) ("%(PDF)" (lambda nil (if (and (eq TeX-engine (quote default)) (or TeX-PDF-mode TeX-DVI-via-PDFTeX)) "pdf" ""))) ("%(PDFout)" (lambda nil (cond ((and (eq TeX-engine (quote xetex)) (not TeX-PDF-mode)) " -no-pdf") ((and (eq TeX-engine (quote luatex)) (not TeX-PDF-mode)) " --output-format=dvi") ((and (eq TeX-engine (quote default)) (not TeX-PDF-mode) TeX-DVI-via-PDFTeX) " \"\\pdfoutput=0 \"") (t "")))) ("%(mode)" (lambda nil (if TeX-interactive-mode "" " -interaction=nonstopmode"))) ("%(o?)" (lambda nil (if (eq TeX-engine (quote omega)) "o" ""))) ("%(tex)" (lambda nil (eval (nth 2 (assq TeX-engine (TeX-engine-alist)))))) ("%(latex)" (lambda nil (eval (nth 3 (assq TeX-engine (TeX-engine-alist)))))) ("%(execopts)" my:ConTeXt-expand-options) ("%S" TeX-source-correlate-expand-options) ("%dS" TeX-source-specials-view-expand-options) ("%cS" TeX-source-specials-view-expand-client) ("%(outpage)" (lambda nil (if TeX-source-correlate-output-page-function (funcall TeX-source-correlate-output-page-function) "1"))) ("%s" file nil t) ("%t" file t t) ("%`" (lambda nil (setq TeX-command-pos t TeX-command-text ""))) (" \"\\" (lambda nil (if (eq TeX-command-pos t) (setq TeX-command-pos pos pos (+ 3 pos)) (setq pos (1+ pos))))) ("\"" (lambda nil (if (numberp TeX-command-pos) (setq TeX-command-text (concat TeX-command-text (substring command TeX-command-pos (1+ pos))) command (concat (substring command 0 TeX-command-pos) (substring command (1+ pos))) pos TeX-command-pos TeX-command-pos t) (setq pos (1+ pos))))) ("%'" (lambda nil (prog1 (if (stringp TeX-command-text) (progn (setq pos (+ (length TeX-command-text) 9) TeX-command-pos (and (string-match " " (funcall file t t)) "\"")) (concat TeX-command-text " \"\\input\"")) (setq TeX-command-pos nil) "") (setq TeX-command-text nil)))) ("%n" TeX-current-line) ("%d" file "dvi" t) ("%f" file "ps" t) ("%o" (lambda nil (funcall file (TeX-output-extension) t))) ("%b" TeX-current-file-name-master-relative) ("%m" preview-create-subdirectory))))
 '(TeX-output-view-style (quote (("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi %dS %d") ("^pdf$" "." "pdfopen -viewer xpdf %s %o %(outpage)") ("^html?$" "." "netscape %o"))))
 '(TeX-parse-self t)
 '(TeX-PDF-mode t)
 '(TeX-save-query nil)
 '(TeX-view-program-selection (cond ((eq system-type (quote windows-nt)) (quote (((output-dvi style-pstricks) "dvips and start") (output-dvi "Yap") (output-pdf "start") (output-html "start")))) (t (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "xpdf") (output-html "xdg-open"))))))
 '(ansi-color-for-comint-mode t)
 '(autopair-autowrap t)
 '(autopair-global-mode 1)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
 '(blink-cursor-mode nil)
 '(bs-alternative-configuration "all-intern-last")
 '(bs-default-configuration "files-and-scratch")
 '(bs-default-sort-name "by mode")
 '(calendar-date-style (quote iso))
 '(calendar-latitude 43.6)
 '(calendar-longitude 1.433333)
 '(calendar-mark-holidays-flag t)
 '(calendar-view-holidays-initially-flag nil)
 '(calendar-week-start-day 1)
 '(color-theme-is-cumulative nil)
 '(column-number-mode t)
 '(comint-prompt-read-only t)
 '(compilation-scroll-output t)
 '(completion-ignored-extensions (append (quote (".bak" "~" "#" ".o" ".obj" ".lib" ".elc" ".exe" ".com" ".zo" ".arj" ".lza" ".lha" ".arc" ".zoo" ".log" ".synctex")) completion-ignored-extensions))
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(custom-buffer-done-kill t)
 '(custom-group-tag-faces nil)
 '(custom-safe-themes (quote ("a3b69ce66b64b5a13865c495168011382c040c7e" "48b99c0a71f0b2b237f9638f07807936b87bf4de" default)))
 '(default-input-method nil)
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(directory-free-space-args "-Pkh")
 '(dired-omit-files "^\\.\\.?$\\|^\\..*$")
 '(display-time-format "[ %H:%M -- %d/%m ]")
 '(display-time-mode t)
 '(european-calendar-style t)
 '(ffap-newfile-prompt t)
 '(fic-background-color nil)
 '(fic-foreground-color "red4")
 '(fill-column 72)
 '(fill-nobreak-predicate (quote (fill-french-nobreak-p)))
 '(font-latex-deactivated-keyword-classes nil)
 '(font-latex-fontify-script t)
 '(font-latex-fontify-sectioning 1.1)
 '(font-latex-is-Emacs20 t)
 '(font-latex-match-bold-command-keywords (quote (("usemodule" "[") ("starttext" "") ("stoptext" ""))))
 '(font-latex-match-bold-declaration-keywords (quote (("bold" ""))))
 '(font-latex-match-function-keywords (quote (("startalign" "[") ("stopalign" "") ("startenumerate" "") ("stopenumerate" "") ("framed" "[{") ("doifmode" "{{") ("doifmodeelse" "{{{") ("doifnotmode" "{{") ("doiftext" "{{") ("doiftextelse" "{{{") ("definepagebreak" "[[") ("definefontfeature" "[[[") ("startsectionblockenvironment" "[") ("stopsectionblockenvironment" "") ("startsetups" "") ("stopsetups" "") ("setups" "[{") ("setlayer" "[[{") ("setlayerframed" "[[[{") ("definelayer" "[[") ("defineselector" "[[") ("setupselector" "[[") ("definepagebreack" "[[") ("placeheadtext" "[") ("placerawheadtext" "[") ("placeheadnumber" "[") ("placerawheadnumber" "[") ("definestructureconversionset" "[[[") ("setupuserpagenumber" "[") ("paperwidth" "") ("paperheight" "") ("placetable" "[[{") ("usetypescript" "[") ("definetypeface" "[[[[[[") ("starttypescript" "[") ("stoptypescript" "") ("blank" "[") ("setupitemize" "[[") ("completecontent" "[") ("placecontent" "[") ("placebookmarks" "[[") ("placecombinedlist" "[[") ("placefigure" "[[{") ("placealgorithme" "[[{") ("externalfigure" "[[") ("placefootnotes" "[") ("placeformula" "[") ("placelegend" "{{") ("placelist" "[[") ("placelistoffloats" "") ("placelistofsorts" "") ("placelistofsynonyms" "") ("placelocalfootnotes" "[") ("placelogos" "[") ("placeongrid" "[{") ("placeontopofeachother" "{{") ("placereferencelist" "[") ("placeregister" "[[") ("placerule" "[") ("placesidebyside" "{{") ("placesubformula" "[") ("placetextvariable" "[") ("startalignment" "[[") ("startbackground" "[[") ("startbuffer" "[[") ("startcolor" "[[") ("startcolumns" "[[") ("startcombination" "[[") ("startcomment" "[[") ("startcomponent" "[[") ("startdescription" "[[") ("startdocument" "[[") ("startenumeration" "[[") ("startenvironment" "[[") ("startfact" "[[") ("startfigure" "[[") ("startfloattext" "[[") ("startformula" "[[") ("startframedtext" "[[") ("starthiding" "[[") ("startinteractionmenu" "[[") ("startitemize" "[") ("starttextitemize" "[") ("stoptextitemize" "[") ("starttextenumerate" "[") ("stoptextenumerate" "[") ("startlegend" "[[") ("startline" "[[") ("startlinecorrection" "[[") ("startlinenumbering" "[[") ("startlines" "[[") ("startlocal" "[[") ("startlocalenvironment" "[[") ("startlocalfootnotes" "[[") ("startmakeup" "[[") ("startmarginblock" "[[") ("startmarginrule" "[[") ("startnamemakeup" "[[") ("startnarrower" "[[") ("startopposite" "[[") ("startoverlay" "[[") ("startoverview" "[[") ("startpacked" "[[") ("startparagraph" "[[") ("startpositioning" "[[") ("startpostponing" "[[") ("startproduct" "[[") ("startprofile" "[[") ("startproject" "[[") ("startquotation" "[[") ("startregister" "[[") ("startsymbolset" "[[") ("startsynchronization" "[[") ("starttable" "[[") ("starttables" "[[") ("starttabulate" "[[") ("starttextrule" "[[") ("starttyping" "[[") ("startunpacked" "[[") ("startversion" "[[") ("stopalignment" "") ("stopbackground" "") ("stopbuffer" "") ("stopcolor" "") ("stopcolumns" "") ("stopcombination" "") ("stopcomment" "") ("stopcomponent" "") ("stopdescription" "") ("stopdocument" "") ("stopenumeration" "") ("stopenvironment" "") ("stopfact" "") ("stopfigure" "") ("stopfloattext" "") ("stopformula" "") ("stopframedtext" "") ("stophiding" "") ("stopinteractionmenu" "") ("stopitemize" "[") ("stoplegend" "") ("stopline" "") ("stoplinecorrection" "") ("stoplinenumbering" "") ("stoplines" "") ("stoplocal" "") ("stoplocalenvironment" "") ("stoplocalfootnotes" "") ("stopmakeup" "") ("stopmarginblock" "") ("stopmarginrule" "") ("stopnamemakeup" "") ("stopnarrower" "") ("stopopposite" "") ("stopoverlay" "") ("stopoverview" "") ("stoppacked" "") ("stopparagraph" "") ("stoppositioning" "") ("stoppostponing" "") ("stopproduct" "") ("stopprofile" "") ("stopproject" "") ("stopquotation" "") ("stopregister" "") ("stopsymbolset" "") ("stopsynchronization" "") ("stoptable" "") ("stoptables" "") ("stoptabulate" "") ("stoptextrule" "") ("stoptyping" "") ("stopunpacked" "") ("stopversion" "") ("define" "[[") ("defineblank" "[[") ("defineblock" "[[") ("definebodyfont" "[[") ("definebodyfontenvironment" "[[") ("definebuffer" "[[") ("definecolor" "[[") ("definecolorgroup" "[[") ("definecombinedlist" "[[[") ("defineconversion" "[[") ("definedescription" "[[") ("defineenumeration" "[[[") ("definefield" "[[") ("definefieldstack" "[[") ("definefiguresymbol" "[[") ("definefloat" "[[") ("definefont" "[[") ("defineframed" "[[") ("defineframedtext" "[[") ("definehead" "[[") ("defineindenting" "[[") ("defineinteractionmenu" "[[") ("defineinteractionmenu (2)" "[[") ("definelabel" "[[") ("definelist" "[[[") ("definelogo" "[[") ("definemakeup" "[[") ("definemarking" "[[") ("defineoutput" "[[") ("defineoverlay" "[[") ("definepalet" "[[") ("definepapersize" "[[") ("defineparagraphs" "[[") ("defineprofile" "[[") ("defineprogram" "[[") ("definerawfont" "[[") ("definereference" "[[") ("definereferenceformat" "[[") ("definereferencelist" "[[") ("defineregister" "[[") ("definerule" "[[") ("definesection" "[[") ("definesectionblock" "[[") ("definesorting" "[[") ("definestartstop" "[[") ("definesubfield" "[[") ("definesymbol" "[[") ("definesynonyms" "[[[") ("definetabletemplate" "[[") ("definetabulate" "[[") ("definetext" "[[") ("definetextposition" "[[") ("definetextvariable" "[[") ("definetype" "[[") ("definetyping" "[[") ("defineversion" "[[") ("setuppapersize" "[[") ("setuplayout" "[") ("setupenumerations" "[[") ("setupalign" "[[") ("setuparranging" "[[") ("setupbackground" "[[") ("setupbackgrounds" "[[") ("setupblackrules" "[[") ("setupblank" "[[") ("setupblock" "[[") ("setupbodyfont" "[[") ("setupbodyfontenvironment" "[[") ("setupbottom" "[[") ("setupbottomtexts" "[[[") ("setupbuffer" "[[") ("setupbuttons" "[[") ("setupcapitals" "[[") ("setupcaption" "[[") ("setupcaptions" "[[") ("setupclipping" "[[") ("setupcolor" "[[") ("setupcolors" "[[") ("setupcolumns" "[[") ("setupcombinations" "[[") ("setupcombinedlist" "[[") ("setupcomment" "[[") ("setupdescriptions" "[[") ("setupenumerations" "[[") ("setupexternalfigures" "[[") ("setupfield" "[[") ("setupfields" "[[") ("setupfillinlines" "[[") ("setupfillinrules" "[[") ("setupfloat" "[[") ("setupfloats" "[[") ("setupfloatsplitting" "[[") ("setupfooter" "[[") ("setupfootertexts" "[[[[") ("setupfootnotedefinition" "[[") ("setupfootnotes" "[[") ("setupforms" "[[") ("setupformulae" "[[") ("setupframed" "[[") ("setupframedtexts" "[[") ("setuphead" "[[") ("setupheader" "[[") ("setupheadertexts" "[[[[") ("setupheadnumber" "[[") ("setupheads" "[[") ("setupheadtext" "[[") ("setuphyphenmark" "[[") ("setupindentations" "[[") ("setupindenting" "[[") ("setupinmargin" "[[") ("setupinteraction" "[[") ("setupinteractionbar" "[[") ("setupinteractionscreen" "[[") ("setupinterlinespace" "[[") ("setupitemgroup" "[[") ("setupitems" "[[") ("setuplabeltext" "[[") ("setuplanguage" "[[") ("setuplayout" "[[") ("setuplegend" "[[") ("setuplinenumbering" "[[") ("setuplines" "[[") ("setuplinewidth" "[[") ("setuplist" "[[") ("setupmakeup" "[[") ("setupmarginblocks" "[[") ("setupmarginrules" "[[") ("setupmarking" "[[") ("setupnarrower" "[[") ("setupnumbering" "[[") ("setupoppositeplacing" "[[") ("setupoutput" "[[") ("setuppagenumber" "[[") ("setuppagenumbering" "[[") ("setuppagetransitions" "[[") ("setuppalet" "[[") ("setuppaper" "[[") ("setuppapersize" "[[") ("setupparagraphnumbering" "[[") ("setupparagraphs" "[[") ("setuppositioning" "[[") ("setupprofiles" "[[") ("setupprograms" "[[") ("setuppublications" "[[") ("setupquote" "[[") ("setupreferencelist" "[[") ("setupreferencing" "[[") ("setupregister" "[[") ("setuprotate" "[[") ("setuprule" "[[") ("setupscreens" "[[") ("setupsection" "[[") ("setupsectionblock" "[[") ("setupsorting" "[[") ("setupspacing" "[[") ("setupstrut" "[[") ("setupsubpagenumber" "[[") ("setupsymbolset" "[[") ("setupsynchronization" "[[") ("setupsynchronizationbar" "[[") ("setupsynonyms" "[[") ("setupsystem" "[[") ("setuptab" "[[") ("setuptables" "[[") ("setuptabulate" "[[") ("setuptext" "[[") ("setuptextposition" "[[") ("setuptextrules" "[[") ("setuptexttexts" "[[") ("setuptextvariable" "[[") ("setupthinrules" "[[") ("setuptolerance" "[[") ("setuptop" "[[") ("setuptoptexts" "[[[") ("setuptype" "[[") ("setuptyping" "[[") ("setupunderbar" "[[") ("setupurl" "[[") ("setupversions" "[[") ("setupwhitespace" "[[") ("showsetups" "[[") ("Use" "{[{"))))
 '(font-latex-match-italic-command-keywords nil)
 '(font-latex-match-italic-declaration-keywords (quote (("italic" "") ("slanted" "") ("english" ""))))
 '(font-latex-match-math-command-keywords (quote (("math" "{") ("mathematics" "{"))))
 '(font-latex-match-reference-keywords (quote (("NN" "[") ("NC" "[") ("AR" "[") ("FR" "[") ("MR" "[") ("LR" "[") ("HL" "[") ("NR" "[") ("SR" "[") ("DL" "[") ("DC" "[") ("about" "[|{{[") ("at" "[|{{[") ("atpage" "[") ("in" "[|{{[") ("inchp" "[") ("insec" "[") ("infig" "[") ("intab" "[") ("ineq" "[") ("ref" "[[") ("reference" "[["))))
 '(font-latex-match-sectioning-0-keywords (quote (("startpart" "[[") ("stoppart" "") ("startstructurelevel" "[[") ("stopstructurelevel" ""))))
 '(font-latex-match-sectioning-1-keywords (quote (("title" "[{") ("startchapter" "[[") ("stopchapter"))))
 '(font-latex-match-sectioning-2-keywords (quote (("subject" "[{") ("startsection" "[[") ("stopsection"))))
 '(font-latex-match-sectioning-3-keywords (quote (("subsubject" "[{") ("startsubsection" "[[") ("stopsubsection"))))
 '(font-latex-match-sectioning-4-keywords (quote (("subsubsubject" "[{") ("startsubsubsection" "[[") ("stopsubsubsection"))))
 '(font-latex-match-sectioning-5-keywords (quote (("subsubsubsubject" "[{") ("startsubsubsubsection" "[[") ("stopsubsubsubsection"))))
 '(font-latex-match-slide-title-keywords nil)
 '(font-latex-match-textual-keywords nil)
 '(font-latex-match-type-command-keywords nil)
 '(font-latex-match-type-declaration-keywords nil)
 '(font-latex-match-variable-keywords nil)
 '(font-latex-match-warning-keywords (quote (("break" "") ("unprotect" "") ("protect" "") ("todo" "{"))))
 '(font-lock-display-type (quote color))
 '(font-lock-global-modes t)
 '(fringe-mode 0 nil (fringe))
 '(global-auto-revert-mode t)
 '(global-visual-line-mode nil)
 '(grep-command "grep -nH -e ")
 '(grep-find-command "find . -type f -print0 | xargs -0 -e grep -nH -e ")
 '(grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e grep <C> -nH -e <R>")
 '(grep-highlight-matches t)
 '(grep-scroll-output t)
 '(grep-template "grep <C> -nH -e <R> <F>")
 '(grep-use-null-device nil)
 '(help-window-select t)
 '(holiday-local-holidays (quote ((holiday-fixed 7 14 "Bastille Day"))))
 '(holiday-other-holidays (quote ((holiday-fixed 5 8 "WWII") (holiday-fixed 11 11 "WWI"))))
 '(ibuffer-compressed-file-name-regexp "\\.\\(arj\\|bgz\\|bz2\\|gz\\|lzh\\|taz\\|tbz\\|tgz\\|zip\\|z\\)$")
 '(ibuffer-default-shrink-to-minimum-size nil)
 '(ibuffer-default-sorting-mode (quote major-mode))
 '(ibuffer-old-time 24)
 '(ibuffer-saved-filters (quote (("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode) (mode . lua-mode) (mode . f90) (mode . fortran)))) ("TeX/LaTeX/ConTeXt" ((or (mode . tex) (mode . latex) (mode . TeX) (mode . LaTeX) (mode . context)))))))
 '(ibuffer-title-face (quote ibuffer-header-face))
 '(image-dired-main-image-directory "~/pictures/")
 '(indent-tabs-mode nil)
 '(inhibit-local-menu-bar-menus t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(iswitchb-buffer-ignore (quote ("^\\( \\|*\\)")))
 '(iswitchb-max-to-show 6)
 '(iswitchb-mode t)
 '(iswitchb-use-virtual-buffers t nil (recentf))
 '(kill-read-only-ok t)
 '(kill-whole-line t)
 '(menu-bar-mode nil)
 '(msb-files-by-directory nil)
 '(msb-menu-cond (quote (((and (boundp (quote server-buffer-clients)) server-buffer-clients (quote multi)) 3030 "Clients (%d)") ((and msb-display-invisible-buffers-p (msb-invisible-buffer-p) (quote multi)) 3090 "Invisible buffers (%d)") ((eq major-mode (quote dired-mode)) 2010 "Dired (%d)" msb-dired-item-handler msb-sort-by-directory) ((eq major-mode (quote Man-mode)) 4090 "Manuals (%d)") ((eq major-mode (quote w3-mode)) 4020 "WWW (%d)") ((or (memq major-mode (quote (rmail-mode rmail-edit-mode vm-summary-mode vm-mode mail-mode))) (memq major-mode (quote (mh-letter-mode mh-show-mode mh-folder-mode))) (memq major-mode (quote (gnus-summary-mode message-mode gnus-group-mode gnus-article-mode score-mode gnus-browse-killed-mode)))) 4010 "Mail (%d)") ((not buffer-file-name) 4099 "Buffers (%d)") ((quote no-multi) 1099 "Files (%d)"))))
 '(msb-mode t)
 '(paren-match-face (quote show-paren-match))
 '(paren-mismatch-face (quote show-paren-mismatch))
 '(parse-sexp-ignore-comments t)
 '(partial-completion-mode t)
 '(pr-gs-switches (quote ("-q -dNOPAUSE -I/usr/share/ghostscript/8.64")))
 '(preview-auto-cache-preamble t)
 '(preview-scale-function 1.4)
 '(proced-auto-update-flag t)
 '(proced-tree-flag t)
 '(read-buffer-function (quote iswitchb-read-buffer))
 '(read-file-name-completion-ignore-case t)
 '(recentf-arrange-rules (quote (("Elisp (%d)" ".\\.\\(el\\|emacs\\)\\'") ("Java (%d)" ".\\.java\\'") ("C/C++ (%d)" ".\\.c\\(pp\\)?\\'") ("Lua (%d)" ".\\.lua\\'") ("Archives (%d)" ".\\.\\(tar\\.\\(gz\\|bz2\\)\\|tgz\\|tbz\\)\\'") ("TeX/LaTeX/ConTeXt (%d)" ".\\.\\(\\(la\\|b\\)?tex\\|ltx\\)\\'"))))
 '(recentf-filter-changer-alist (quote ((recentf-arrange-by-rule . "Grouped by Custom Rules") (recentf-arrange-by-dir . "Grouped by Directory") (recentf-arrange-by-mode . "Grouped by Mode"))))
 '(recentf-max-saved-items 50)
 '(recentf-menu-append-commands-flag nil)
 '(recentf-menu-filter (quote recentf-filter-changer))
 '(recentf-menu-open-all-flag t)
 '(recentf-mode t)
 '(require-final-newline t)
 '(rlogin-program "ssh")
 '(rlogin-remote-user (user-login-name))
 '(safe-local-eval-forms (quote ((add-hook (quote write-file-functions) (quote time-stamp)))))
 '(safe-local-variable-values (quote ((TeX-source-correlate-method . source-specials) (TeX-source-correlate-method-active . source-specials))))
 '(scalable-fonts-allowed t)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(tab-always-indent (quote complete))
 '(tab-width 2)
 '(temp-buffer-max-height 15)
 '(temp-buffer-resize-mode t)
 '(tm/use-open-next-line nil)
 '(tool-bar-mode nil)
 '(track-eol nil)
 '(tramp-backup-directory-alist backup-directory-alist)
 '(tramp-default-user (user-login-name))
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(version-control t)
 '(view-inhibit-help-message t)
 '(view-read-only t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(windmove-wrap-around t)
 '(window-min-height 4)
 '(word-wrap t)
 '(yank-pop-change-selection t))

;>> custom-set-variable (end) ------------------------------------- >>
;<< -- custom-set-faces       ------------------------------------- >>

(make-face 'ibuffer-header-face)

(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil
                :strike-through nil :overline nil :underline nil
                :slant normal :weight normal :height 80 :width normal
                :foundry "unknown" :family "DejaVu Sans Mono")))))

;>> custom-set-faces ---------------------------------------------- >>

;<< -- (re)set some variables ------------------------------------- >>

(defalias 'yes-or-no-p 'y-or-n-p)
(defcustom explicit-shell-file-name "/bin/bash" ""
  :type 'string
  :group 'acme)
(setq-default filladapt-mode t filladapt-mode-line-string nil)

;>> (re)set some variables (end) ---------------------------------- >>
;<< -- enable some goodies    ------------------------------------- >>

(when (require 'mic-paren nil t)
  (paren-activate))

(when (require 'winner nil t)
  (winner-mode t))

(when (require 'dired-x nil t)
  (add-hook 'dired-mode-hook 'dired-omit-mode))

;>> enable some goodies (end) ------------------------------------- >>
;<< -- load some packages     ------------------------------------- >>

(defcustom packages-to-load
  '("drag-stuff" "fic-mode" "filladapt" "gnuplot"
    "gnuplot-gui" "lua-mode" "rainbow-mode" "textmate")
  "List of packages to load automatically."
  :type '(repeat string)
  :group 'acme)

(dolist (package packages-to-load)
  (require (intern package) nil t))

;>> load some packages (end) -------------------------------------- >>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;<< -- help-mode     ---------------------------------------------- >>

(add-hook
 'help-mode-hook
 (lambda nil
   (local-set-key (kbd "<backtab>") nil)
   (local-set-key (kbd "<M-left>")  'help-go-back)
   (local-set-key (kbd "<M-right>") 'help-go-forward)))

;>> help-mode (end) ----------------------------------------------- >>
;<< -- folding-mode  ---------------------------------------------- >>

(when (require 'folding nil t)
  (defcustom add-folding-mode-to
    '(("emacs-lisp" ";<<"  ";>>")
      ("lua"        "--<<" "-->>"))
    "List of modes where to activate `folding-mode' automatically."
    :type '(repeat (repeat string))
    :group 'acme)

  (dolist (mode add-folding-mode-to)
    (let ((beg (nth 1 mode))
          (end (nth 2 mode))
          (hook (intern (concat (nth 0 mode) "-mode-hook")))
          (mode (intern (concat (nth 0 mode) "-mode"))))
      (add-hook hook 'folding-mode)
      (folding-add-to-marks-list mode beg end)))

  (add-hook
   'folding-mode-hook
   (lambda nil
     (define-key folding-mode-map (kbd "M-g") nil) ; make it a prefix
     (local-set-key (kbd "M-g M-g") 'folding-goto-line)
     (local-set-key (kbd "C-c f")   nil) ; make it a prefix
     (local-set-key (kbd "M-ESC")   'folding-toggle-show-hide)
     (local-set-key (kbd "C-c f s") 'folding-show-current-subtree)
     (local-set-key (kbd "C-c f h") 'folding-hide-current-subtree)
     (local-set-key (kbd "C-c f o") 'folding-open-buffer)
     (local-set-key (kbd "C-c f c") 'folding-whole-buffer))))

;>> folding-mode (end) -------------------------------------------- >>
;<< -- lua-mode      ---------------------------------------------- >>

(when (require 'lua-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'auto-mode-alist '("\\.mlua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

  (add-hook
   'lua-mode-hook
   (lambda nil
     (tm/minor-mode-on)
     (setq lua-indent-level 2)
     (setq lua-indent-whitespace " ")
     (local-set-key (kbd " ") (lambda nil (interactive) (insert " ")))
     (local-set-key (kbd "C-c C-r") 'lua-restart-with-whole-file)
     (local-set-key (kbd "C-c s") 'lua-show-process-buffer)
     (local-set-key (kbd "C-c h") 'lua-hide-process-buffer))))

;>> lua-mode (end) ------------------------------------------------ >>

;<< -- latex-mode    ---------------------------------------------- >>

(when (require 'tex-site nil t)
  (defvar TeX-shell-escape-mode nil
    "*Allow shell escapes or not.")
  (defvar TeX-XeTeX-mode nil
    "Ugly hack used to make PDF ConTeXt work.")
  (defvar my:display-math-mode nil
    "Wether to type display math or nat.")
  (defvar my:display-math-beginning "\n\\[\n"
    "The string used to insert BEFORE a math display.")
  (defvar my:display-math-end "\n\\]\n"
    "The string used to insert AFTER a math display.")
  (defvar my:inline-math-beginning "\\( "
    "The string used to insert BEFORE a math inline.")
  (defvar my:inline-math-end " \\)"
    "The string used to insert AFTER a math inline.")

  (defvar cdlatex-math-modify-prefix 176)
  (require 'cdlatex nil t)

  (defun my:latex-unbreakable-space ()
    "Insère '~' pour inserer une espace insécable sous LaTeX."
    (interactive)
    (insert "~"))

  (defun my:insert-math-formula ()
    "Insère « \\(|\\) » (ou « \\[
|
\\] ») avec le cruseur à la place du « | »."
    (interactive)
    (if (eq last-command this-command)
        (progn
          (delete-char 3)
          (delete-char -3)
          (setq my:display-math-mode (not my:display-math-mode))))
    (if my:display-math-mode
        (insert "\\[

\\]")
      (insert "\\(  \\)"))
    (goto-char (- (point) 3)))


  (defun my:toggle-shell-escape ()
    (interactive)
    "(Dés)active l'option de compilation de (La)TeX \"-shell-escape\""
    (setq TeX-shell-escape-mode (not TeX-shell-escape-mode))
    (message (concat "TeX shell escapes " (if TeX-shell-escape-mode "activated" "deactivated"))))

  (eval-after-load "tex"
    '(progn
       (require 'font-latex)
       (require 'reftex)))

  (eval-after-load "latex"
    '(progn
       (require 'font-latex)
       (require 'reftex)
       (require 'preview-latex)
       (require 'bib-cite)))

  (defun my-hooks:latex-mode ()
    "Setup LaTeX AUCTeX mode."
    ;; (turn-on-bib-cite) ?
    (turn-on-reftex)
    ;; (turn-on-filladapt-mode)
    (turn-on-auto-fill)
    (imenu-add-menubar-index)
    (local-set-key (kbd "M-$")         'my:insert-math-formula)
    (local-set-key (kbd "M-q")         'LaTeX-fill-paragraph)
    (local-set-key (kbd "<f9>")        'TeX-next-error)
    (local-set-key (kbd "C-c C-t C-t") 'my:toggle-shell-escape)
    (local-set-key (kbd "<")           'self-insert-command)
    (local-set-key (kbd ">")           'self-insert-command)
    (face-remap-add-relative 'font-lock-function-name-face
                             :foreground "OliveDrab")
    (folding-mode t))

  (add-hook 'bibtex-mode-hook 'BibTeX-auto-store)

  (put 'TeX-master 'safe-local-variable 'stringp)
  (folding-add-to-marks-list 'latex-mode "%<<" "%>>")
  (add-hook 'LaTeX-mode-hook 'my-hooks:latex-mode))

;>> latex-mode ---------------------------------------------------- >>
;<< -- context-mode  ---------------------------------------------- >>

(when (require 'tex-site nil t)
  (defun my:tex-unbreakable-space ()
    "Insère '~' pour inserer une espace insécable sous (La|Con)TeX(|t)."
    (interactive)
    (insert "~"))

  (defcustom ConTeXt-modes-list nil
    "List of modes to run ConTeXt with."
    :group 'AUCTeX
    :type '(repeat (string)))

  (defcustom ConTeXt-verbose t
    "Switch to turn lots of information at the end of a run."
    :group 'AUCTeX
    :type 'boolean)

  (defcustom ConTeXt-use-makefile t
    "Use a Makefile to compile ConTeXt."
    :group 'AUCTeX
    :type 'boolean)

  (defcustom ConTeXt-use-beta t
    "Use the beta branch of ConTeXt."
    :group 'AUCTeX
    :type 'boolean)

  ;; (if ConTeXt-use-beta
  ;;     (setenv "PATH" "/DATA/context-minimals/tex/texmf-linux/bin:$PATH" t))

  (eval-after-load "context"
    '(progn
       (require 'font-latex)
       (require 'reftex)))

  (defun my:ConTeXt-expand-options ()
    "Expand options for texexec command."
    (concat
     "--autogenerate "
     (unless ConTeXt-verbose
       (format "--nostats "))
     (if TeX-PDF-mode
         (format "--pdf "))
     (unless (eq ConTeXt-current-interface "en")
       (format "--interface=%s " ConTeXt-current-interface))
     (unless TeX-interactive-mode
       ConTeXt-texexec-option-nonstop)
     (if (and ConTeXt-modes-list (not (eq ConTeXt-modes-list "")))
         (progn
           (defvar modes "")
           (setq modes "")
           (dolist (mode ConTeXt-modes-list)
             (setq modes (concat modes mode ",")))
           (format "--mode=%s " modes)))))

  (eval-and-compile
    (defmacro my-context:insert (name strbeg strend)
      `(defun ,(intern (concat "my:enclose-in-" name)) (beg end)
         (interactive "r")
         (if (not (region-active-p))
             (progn (setq beg (point))
                    (setq end (point))))
         (goto-char end)
         (insert ,strend)
         (goto-char beg)
         (insert ,strbeg))))

  (defvar etexshow-xml-files-alist nil)
  (when (require 'etexshow nil t)
    (setq etexshow-xml-files-alist
          `((,(concat lisp-root "etexshow/cont-en.xml") . ,(concat lisp-root "etexshow/cont-en.cache"))
            (,(concat lisp-root "etexshow/mycommands.xml") . ,(concat lisp-root "etexshow/mycommands.cache"))))
    (setq etexshow-comment-file (concat lisp-root "etexshow/cont-en-comments.xml"))
    (add-hook 'etexshow-mode-hook
              '(lambda () (local-set-key (kbd "<f7>") 'etexshow-quit))))

  (my-context:insert "doubt"   "\\doute{" "}")
  (my-context:insert "english" "{\\english " "}")
  (my-context:insert "quote"   "« " " »")
  (my-context:insert "inline"  "|<|" "|>|")

  (defun my-hooks:context-mode ()
    (load "texmathp" t t)

    (when (load "etexshow" t t)
      (defvar etexshow-xml-files-alist
        '(("~/emacs/lisp/etexshow/cont-en.xml" .
           "~/emacs/lisp/etexshow/cont-en.cache")
          ("~/emacs/lisp/etexshow/mycommands.xml" .
           "~/emacs/lisp/etexshow/mycommands.cache")))
      (add-hook 'etexshow-mode-hook
                '(lambda () (local-set-key (kbd "<f7>") 'etexshow-quit))))

    ; (local-set-key (kbd "C-c C-c") nil)
    (local-set-key (kbd "C-c C-c")
                   (lambda ()
                     (interactive) (save-buffer) (my:compile "make -k")))
    (local-set-key (kbd "C-c C-v")
                   (lambda ()
                     (interactive)
                     (save-buffer)
                     (async-shell-command
                      (concat "make -C.. "
                              (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))))

    ; insert strings in the buffer)
    (local-set-key (kbd "C-c i")   nil)
    (local-set-key (kbd "C-c i d") 'my:enclose-in-doubt)
    (local-set-key (kbd "C-c i e") 'my:enclose-in-english)
    (local-set-key (kbd "C-c i q") 'my:enclose-in-quote)
    (local-set-key (kbd "C-c i i") 'my:enclose-in-inline)
    (local-set-key (kbd " ")       'my:tex-unbreakable-space)

    ; some help for the commands)
    (local-set-key (kbd "<f7>")    'etexshow-cmd)
    (local-set-key (kbd "<S-f7>")  'etexshow)

    ; other)
    (local-set-key (kbd "M-q")     'LaTeX-fill-paragraph)

    (setq TeX-engine 'luatex)
    (setq TeX-command-default "ConTeXt Full")
    (auto-fill-mode t)
    (folding-mode t)
    (setq fill-column 80))

  (defun ConTeXt-switch-makefile-AUCTeX ()
    (interactive)
    (if (not ConTeXt-use-makefile)
        (local-set-key (kbd "C-c C-c")
                       (lambda ()
                         (interactive) (save-buffer) (my:compile "make -k")))
      (local-set-key (kbd "C-c C-c") 'TeX-command-master))
    (setq ConTeXt-use-makefile (not ConTeXt-use-makefile)))

  (put 'ConTeXt-modes-list 'safe-local-variable 'listp)
  (put 'ConTeXt-verbose 'safe-local-variable 'booleanp)
  (put 'ConTeXt-use-beta 'safe-local-variable 'booleanp)

  (folding-add-to-marks-list 'context-mode "%<<" "%>>")
  (add-hook 'ConTeXt-mode-hook 'my-hooks:context-mode))

;>> context-mode -------------------------------------------------- >>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;<< -- useful functions      -------------------------------------- >>

; we may get rid of the following color functions
;<< ---- colors functions ----------------------------------------- >>

;; ** Helper functions **
(defun simple-make-face (spec &optional face-name)
  "Get a symbol or a string SPEC, and make it into a face if it doesn't exist.
In order for it to properly create the face, the following naming convention
must be used:
    attr[-attr][...]
Example: (simple-make-face 'yellow/blue4-bold) creates and returns an
appropriate face named `yellow/blue4-bold'.  (Actually, the color spec can be
anywhere.)

Attribute can be one of: ultra_condensed extra_condensed condensed
semi_condensed semi_expanded expanded extra_expanded ultra_expanded ultra_bold
extra_bold bold semi_bold semi_light light extra_light ultra_light underline
ununderline overline unoverline inverse uninverse italic oblique reverse_italic
reverse_oblique.  It can also be `normal', which will be used for width,
weight, and slant (but attributes are processed from left to right).
\(See `set-face-attribute'.)

Attributes can also be in these forms:
* `fgcolor/bgcolor', where each color can be
  - a color name,
  - `hexH' (H is some hex color), or `hH', or `#H'
  - `*' or `default', or just empty
* `box[N]' for a box with width N, defaults to 1
* `big[N]', `small[N]' for a multiplier or divider of N0%, defaults to 25%
* `scale[N]' (for a scale of N%)

An optional argument FACE-NAME will make it be defined as the result face, and
force the face to be modified if it exists (good for setting properties of
existing faces)."
  (let* ((spec  (if (stringp spec) spec (symbol-name spec)))
         (spec  (if (equal "" spec) (error "Empty face spec") spec))
         (face  (or face-name (intern spec)))
         (attrs (split-string spec "-"))
         (attrs (mapcar (lambda (a) (replace-regexp-in-string "_" "-" a t))
                        attrs))
         (error nil))
    ;; optimize re-generating a simple face with same specs
    (unless (and (not face-name) (equal spec (get face 'simple-face-spec)))
      (unless (memq face (face-list)) (make-face face))
      (save-match-data
        (dolist (attr attrs) (simple-set-face-attribute face attr)))
      (unless face-name (put face 'simple-face-spec spec)))
    face))

(defun simple-set-face-attribute (face attr)
  (let* ((a (intern attr))
         (as (cond ((eq a 'normal)
                    (list :width a :weight a :slant a))
                   ((memq a '(ultra-condensed extra-condensed condensed
					      semi-condensed semi-expanded expanded
					      extra-expanded ultra-expanded))
                    (list :width a))
                   ((memq a '(ultra-bold extra-bold bold semi-bold
					 semi-light light extra-light ultra-light))
                    (list :weight a))
                   ((memq a '(italic oblique reverse-italic reverse-oblique))
                    (list :slant a))
                   ((eq a 'underline)   '(:underline t))
                   ((eq a 'ununderline) '(:underline nil))
                   ((eq a 'overline)    '(:overline t))
                   ((eq a 'unoverline)  '(:overline nil))
                   ((eq a 'inverse)     '(:inverse t))
                   ((eq a 'uninverse)   '(:inverse nil))
                   (t (simple-face-parse-compound-attr attr)))))
    (message "%s -- %s" face as)
    (apply 'set-face-attribute face nil as)))

(defun simple-face-parse-compound-attr (attr)
  (cond
   ((string-match "\\`\\(big\\|small\\|scale\\)\\([0-9]+\\)?\\'" attr)
    (let* ((op    (match-string 1 attr))
	   (arg   (match-string 2 attr))
	   (arg   (and arg (string-to-number arg)))
	   (scale (/ (if (equal op "scale")
			 (or arg 100)
		       (+ 100 (if arg (* 10 arg) 25)))
		     100.0))
	   (scale (if (equal op "small") (/ 1.0 scale) scale)))
      (list :height scale)))
   ((string-match "\\`box\\([0-9]+\\)?\\'" attr)
    (list :box (if (match-beginning 1)
		   (list :line-width (string-to-number (match-string 1 attr)))
		 t)))
   ;; split `.../...' color spec to `.../' and `/...'
   ((string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" attr)
    (let ((fg (concat (match-string 1 attr) "/"))
	  (bg (concat "/" (match-string 2 attr))))
      (append (simple-face-parse-compound-attr fg)
	      (simple-face-parse-compound-attr bg))))
   ;; default colors in several forms, do nothing
   ((member attr '("" "/" "*" "*/" "/*" "default" "default/" "/default"))
    nil)
   ;; fg/bg colors
   ((and (string-match
	  "\\`\\(/\\)?\\(#\\|[hH]\\|hex\\|HEX\\)?\\([^/]*\\)\\(/\\)?\\'" attr)
	 ;; only `.../' or `/...'
	 (if (match-beginning 1) (not (match-beginning 4)) (match-beginning 4)))
    (let* ((rgb   (match-beginning 2))
	   (color (match-string 3 attr))
	   (color (if rgb (concat "#" color) color)))
      (list (if (match-beginning 1) :background :foreground) color)))
   ;; some color with no `/', used as fg
   ((string-match "\\`\\(?:#\\|[hH]\\|hex\\|HEX\\)\\([^/]*\\)\\'" attr)
    (list :foreground (concat "#" (match-string 1 attr))))
   ((assoc (downcase attr) color-name-rgb-alist) ; (defined-colors) better?
    (list :foreground attr))
   (t (error "Bad component in face spec: %S" attr))))

(defun simple-make-face-if-undefined (face)
  (let ((face (if (stringp face) (intern face) face)))
    (if (facep face) face (simple-make-face face))))

(defvar read-face-history nil
  "History variable for reading faces and colors.")

;; Redefined from "faces.el"
(defun read-face-name (prompt &optional string-describing-default multiple)
  "Read a face, defaulting to the face or faces on the char after point.
If it has the property `read-face-name', that overrides the `face' property.
PROMPT should be a string that describes what the caller will do with the face;
it should not end in a space.
STRING-DESCRIBING-DEFAULT should describe what default the caller will use if
the user just types RET; you can omit it.
If MULTIPLE is non-nil, return a list of faces (possibly only one).
Otherwise, return a single face.

** Modified to accept anything, and use `simple-make-face-if-undefined'."
  (let ((faceprop (or (get-char-property (point) 'read-face-name)
		      (get-char-property (point) 'face)))
        (aliasfaces nil)
        (nonaliasfaces nil)
	faces)
    ;; Try to get a face name from the buffer.
    (if (memq (intern-soft (thing-at-point 'symbol)) (face-list))
	(setq faces (list (intern-soft (thing-at-point 'symbol)))))
    ;; Add the named faces that the `face' property uses.
    (if (and (listp faceprop)
	     ;; Don't treat an attribute spec as a list of faces.
	     (not (keywordp (car faceprop)))
	     (not (memq (car faceprop) '(foreground-color background-color))))
	(dolist (f faceprop)
	  (if (symbolp f)
	      (push f faces)))
      (if (symbolp faceprop)
	  (push faceprop faces)))
    (delete-dups faces)

    ;; Build up the completion tables.
    (mapatoms (lambda (s)
                (if (custom-facep s)
                    (if (get s 'face-alias)
                        (push (symbol-name s) aliasfaces)
                      (push (symbol-name s) nonaliasfaces)))))

    ;; If we only want one, and the default is more than one,
    ;; discard the unwanted ones now.
    (unless multiple
      (if faces
	  (setq faces (list (car faces)))))
    (require 'crm)
    (let* ((input
	    ;; Read the input.
	    (completing-read-multiple
	     (if (or faces string-describing-default)
		 (format "%s (default %s): " prompt
			 (if faces (mapconcat 'symbol-name faces ",")
			   string-describing-default))
	       (format "%s: " prompt))
	     (completion-table-in-turn nonaliasfaces aliasfaces)
	     nil nil nil 'read-face-history
	     (if faces (mapconcat 'symbol-name faces ","))))
	   ;; Canonicalize the output.
	   (output
	    (cond ((or (equal input "") (equal input '("")))
		   faces)
		  ((stringp input) (mapcar 'simple-make-face-if-undefined
                                           (split-string input ", *" t)))
		  ((listp input) (mapcar 'simple-make-face-if-undefined input))
		  (input))))
      ;; Return either a list of faces or just one face.
      (if multiple
	  output
	(car output)))))


;; color functions
(defun set-region-face (beg end face &optional force-overlays dont-make-face)
  "Set the face of a region.
Set the region from BEG to END to have face FACE.

Will use face properties, unless font-lock is active -- in that case it will
use overlays.  If the optional argument FORCE-OVERLAYS is t (prefix argument
when called interactively), will use overlays anyway.  If its value is 'both,
will use both overlays and properties (overlays don't stick in copied text)."
  (interactive (if mark-active
		   (list (region-beginning) (region-end) (read-face-name "Face")
			 (and current-prefix-arg t) nil)
                 (error "Use the mark, luke.")))
  (let* ((face (if dont-make-face face (simple-make-face face)))
         ;; These are taken from `save-buffer-state' - make sure that
         ;; we modify the buffer with no state changes.
         (modified                   (buffer-modified-p))
         (buffer-undo-list           t)
         (inhibit-read-only          t)
         (inhibit-point-motion-hooks t)
         (inhibit-modification-hooks t)
         (use-overlays   (or force-overlays
                             (and font-lock-mode font-lock-defaults)))
         (use-properties (or (not use-overlays) (eq 'both force-overlays)))
         deactivate-mark buffer-file-name buffer-file-truename)
    (when use-overlays
      (let ((overlay (make-overlay beg end)))
        (overlay-put overlay 'set-region-face t)
        (overlay-put overlay 'face face)))
    (when use-properties
      (put-text-property beg end 'face face))
    (unless modified (restore-buffer-modified-p nil))))
(put 'set-region-face 'safe-local-eval-function t)

(defun remove-set-face-overlays ()
  "Removes overlays set by `set-region-face'."
  (interactive)
  (remove-overlays nil nil 'set-region-face t))
(put 'remove-set-face-overlays 'safe-local-eval-function t)

(defun set-regexp-face (regexp face &optional force-overlays)
  "Set the face of all occurrences of REGEXP to FACE.
FACE can be a a list of pairs of a subexp number and its color: (SUB . COLOR).
Uses `set-region-face' to set the faces."
  ;; interactive like `add-color-pattern'
  (interactive (list (read-from-minibuffer
                      "Pattern to highlight: "
                      (and transient-mark-mode mark-active
                           (regexp-quote (buffer-substring-no-properties
                                          (region-beginning) (region-end))))
                      nil nil 'regexp-history)
                     (read-face-name "Face")
                     (and current-prefix-arg t)))
  (let ((face (if (listp face)
		  (progn (dolist (f face) (setcdr f (simple-make-face (cdr f))))
			 face)
                (simple-make-face face))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t nil)
        (if (listp face)
	    (dolist (f face)
	      (set-region-face (match-beginning (car f)) (match-end (car f))
			       (cdr f) force-overlays t))
          (set-region-face (match-beginning 0) (match-end 0) face
                           force-overlays t))))))
(put 'set-regexp-face 'safe-local-eval-function t)

(defvar added-color-patterns nil
  "List of colors added using `add-color-pattern'.")

(defun add-color-pattern (regexp face &optional expnum override &rest more)
  "Add a pattern for this buffer's `font-lock-keywords'.
REGEXP is the pattern to be colored with FACE.  When called programatically - a
third integer argument EXPNUM specifies the regexp to hilight, and a fourth
argument OVERRIDE is the override flag.  Possible MORE arguments will specify
more faces, numbers and flags.

When called interactively, a positive prefix argument will make it an
overriding addition, negative prefix makes it a normal addition, and no prefix
make the style appended over any existing style.  When OVERRIDE is set to
anything other than `t' or `nil' in any way, the new pattern will be added to
the end of `font-lock-keywords'.
Examples:
 (add-color-pattern \"regexp\" 'face)
 (add-color-pattern \"\\\\(r\\\\)egexp\" 'face 1 t)
 (add-color-pattern \"\\\\(r\\\\)egex\\\\(p\\\\)\" 'face1 1 t 'face2 2 nil)"
  (interactive (list (read-from-minibuffer
                      "Pattern to highlight: "
                      (and transient-mark-mode mark-active
                           (regexp-quote (buffer-substring-no-properties
                                          (region-beginning) (region-end))))
                      nil nil 'regexp-history)
                     (read-face-name "Face")))
  (when (interactive-p)
    (setq override (cond ((not current-prefix-arg) 'prepend)
                         ((>= (prefix-numeric-value current-prefix-arg) 0) t)
                         (t nil))
          expnum 0))
  (let* ((face (simple-make-face face)) ; in case of calling from an expression
         (face (list 'quote face))
         (add (list (or expnum 0) face override t)))
    (while (and more (cdr more))
      (let* ((face (simple-make-face (pop more)))
             (new (list (pop more) (list 'quote face) (pop more) t)))
        (nconc add (list new))))
    (push regexp add)
    (make-local-variable 'added-color-patterns)
    (unless (member add added-color-patterns)
      (push add added-color-patterns)
      (font-lock-add-keywords
       nil (list add) (if (memq override '(t nil)) nil t))
      (unless font-lock-defaults
        ;; we have a misbehaved mode, set defaults and turn on again
        ;; another way to fix this is (setq-default font-lock-defaults '(nil))
        (font-lock-mode -1)
        (setq font-lock-defaults '(nil))
        (font-lock-mode 1))
      (font-lock-fontify-buffer))))
(put 'add-color-pattern 'safe-local-eval-function t)

(defun remove-added-color-pattern (&optional n)
  "Remove the a pattern added with `add-color-pattern'.
Automatically removes a single-added pattern, otherwise asks which one to
remove.  With a numeric prefix argumet, remove that number of patterns (last
one added first), if negative removes all."
  (interactive "P")
  (let ((n (and n (prefix-numeric-value n))))
    (cond
     ((null added-color-patterns) (message "No patterns to remove."))
     ((or (and n (< n 0)) (= 1 (length added-color-patterns)))
      (font-lock-remove-keywords nil added-color-patterns)
      (setq added-color-patterns nil))
     (n (while (and added-color-patterns (> n 0))
	  (font-lock-remove-keywords nil (list (pop added-color-patterns)))
	  (setq n (1- n))))
     (t (let* ((history-list (mapcar 'car added-color-patterns))
	       (rem (assoc (completing-read "Pattern to unhighlight: "
					    added-color-patterns nil t
					    (caar added-color-patterns)
					    (cons 'history-list 1))
			   added-color-patterns)))
	  (font-lock-remove-keywords nil (list rem))
	  (setq added-color-patterns (delq rem added-color-patterns))))))
  (font-lock-fontify-buffer))
(put 'remove-added-color-pattern 'safe-local-eval-function t)


; cursor color
(defvar my:set-cursor-color-color "" "")
(defvar my:set-cursor-color-buffer "" "")

(defun my:set-cursor-color-according-to-mode ()
  "Change cursor color according to some minor modes."
  (let ((color
         (if buffer-read-only "purple1"
           (if overwrite-mode "red"
             "#55A096"))))  ;; insert mode
    (unless (and (string= color my:set-cursor-color-color)
                 (string= (buffer-name) my:set-cursor-color-buffer))
      (set-cursor-color (setq my:set-cursor-color-color color))
      (setq my:set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'my:set-cursor-color-according-to-mode)

;>> colors functions (end) ---------------------------------------- >>

; invoke shell with correct colors
(defun my:launch-terminal (terminal)
  "Set correct colors then launch terminal."
  (interactive)
  (defvar term-default-bg-color (face-attribute 'default :background))
  (defvar term-default-fg-color (face-attribute 'default :foreground))
  (term terminal))

; copy/cut line or region
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

; duplicate line or region
(defun my:duplicate-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

; toggle case
(defun my:toggle-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'word))
            pos2 (cdr (bounds-of-thing-at-point 'word))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char pos1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]")
          (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]")
          (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]")
          (put this-command 'state "init caps") )
         (t
          (put this-command 'state "all lower")))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region pos1 pos2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region pos1 pos2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region pos1 pos2) (put this-command 'state "all lower")))))

; more for comments
(defvar my:comment-line-last-col nil)

(defun my:toggle-comment-line (n again)
  "Toggle a comment on current N line(s) (disable line by line)."
  (if comment-start
      (let* ((end    (cond ((or (not comment-end) (equal comment-end "")) "")
                           ((string-match "^ " comment-end) comment-end)
                           (t (concat " " comment-end))))
             (start  (cond ((string-match " $" comment-start) comment-start)
                           ((and (= (length comment-start) 1) (equal end ""))
                            (concat comment-start " "))
                           (t (concat comment-start " "))))
             (qstart (regexp-quote start))
             (qend   (regexp-quote end))
             (col    (and again my:comment-line-last-col))
             (mode   (and again (if col 'comment 'uncomment)))
             (direction (if (< 0 n) 1 -1))
             (n  (abs n)))
        (catch 'done
          (beginning-of-line)
          (if (< direction 0)
              (forward-line -1))
          (while (>= (setq n (1- n)) 0)
            (when (eobp) (throw 'done nil))
            (skip-chars-forward "\t ")
            (unless (eolp)
              (unless mode
                (setq mode (if (looking-at (concat qstart "\\(.*\\)" qend "$"))
                               'uncomment 'comment)))
              (let ((cur (current-column)))
                (cond ((and col (< col cur))
                       (move-to-column col t))
                      ((eq mode 'comment)
                       (setq col cur))))
              (cond ((eq mode 'comment)
                     (insert start) (end-of-line) (insert end))
                    ((eq mode 'uncomment)
                     (when (looking-at (concat qstart "\\(.*\\)" qend "$"))
                       (replace-match "\\1" t)))))
            (forward-line direction))
          (if (< direction 0)
              (forward-line 1)))
        (setq my:comment-line-last-col col))
    (message "Comments not available for this mode")))

(defun my:toggle-comment-line-and-go-down (n)
  "Toggle a comment on current N line(s) (disable line by line)."
  (interactive "p")
  (my:toggle-comment-line n
                            (eq last-command 'my:toggle-comment-line-and-go-down)))

(defun my:go-up-and-toggle-comment-line (n)
  "Toggle a comment on current N line(s) (disable line by line)."
  (interactive "p")
  (my:toggle-comment-line (- n)
                            (eq last-command 'my:go-up-and-toggle-comment-line)))

(autoload 'ps2pdf-from-buffer "ps2pdf" nil t)
(autoload 'ps2pdf-from-region "ps2pdf" nil t)

;>> useful functions (end) ---------------------------------------- >>
;<< -- general keybindings   -------------------------------------- >>

; movements
(global-set-key (kbd "M-p") 'previous-line)
(global-set-key (kbd "M-n") 'next-line)

(global-set-key (kbd "M-g")     nil)
(global-set-key (kbd "M-g M-g") 'goto-line)

; marks
(global-set-key (kbd "M-@")   'cua-set-mark)    ; [`mark-word' >>> "M-+"]
(global-set-key (kbd "M-+")   'mark-word)       ; [ free       <<< "M-@"]
(global-set-key (kbd "M-W")   'kill-region)
(global-set-key (kbd "C-S-w") 'kill-ring-save)
(global-set-key (kbd "M-Y")   'cua-paste)
(global-set-key (kbd "C-y")   'cua-paste)
(global-set-key (kbd "C-S-y") 'cua-paste-pop)

; buffers
(global-set-key (kbd "C-x C-y") 'bs-show)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-à") 'switch-to-buffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer) ; [`kmacro-keymap' >>> "C-x k"]
(global-set-key (kbd "C-x k")   'kmacro-keymap)    ; [`kill-buffer'   >>> "C-x C-k"]

(define-key read-expression-map (kbd "<tab>") 'lisp-complete-symbol)

(global-set-key (kbd "C-x y")   'list-buffers)
(defalias 'list-buffers         'electric-buffer-list)

; isearch
(define-key isearch-mode-map (kbd "C-w") 'isearch-yank-word)
(define-key isearch-mode-map (kbd "C-f") 'isearch-yank-char)
(define-key isearch-mode-map (kbd "C-k") 'isearch-yank-line)
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
(define-key isearch-mode-map (kbd "C-t") 'isearch-toggle-case-fold)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

; miscellaneous
(global-set-key (kbd "C-.")   'undo)
(global-set-key (kbd "RET")   'newline-and-indent)
(global-set-key (kbd "M-j")   'join-line); join with previous line
(global-set-key (kbd "M-J")              ; join with next line
  (lambda nil (interactive) (join-line t)))
(global-set-key (kbd "M-SPC") 'dabbrev-expand) ; [`just-one-space' >>> "M-/"]
(global-set-key (kbd "M-/")   'just-one-space) ; [`dabbrev-expand' >>> "M-SPC"]
(global-set-key (kbd "M-'")   'repeat)
(global-set-key (kbd "C-'")   'repeat)
(global-set-key (kbd "M-U")   'universal-argument) ; was free
(global-set-key (kbd "M-z")   'zap-to-char)

(when (require 'misc nil t)
  (global-set-key (kbd "M-Z") 'zap-up-to-char))

(when (require 'drag-stuff nil t)
  (global-set-key (kbd "C-«") 'drag-stuff-left)
  (global-set-key (kbd "C-»") 'drag-stuff-right)
  (global-set-key (kbd "C-+") 'drag-stuff-up)
  (global-set-key (kbd "C--") 'drag-stuff-down))

; free all function keys
(global-set-key (kbd "<f1>")  nil)
(global-set-key (kbd "<f2>")  nil)
(global-set-key (kbd "<f3>")  nil)
(global-set-key (kbd "<f4>")  nil)
(global-set-key (kbd "<f5>")  nil)
(global-set-key (kbd "<f6>")  nil)
(global-set-key (kbd "<f7>")  nil)
(global-set-key (kbd "<f8>")  nil)
(global-set-key (kbd "<f9>")  nil)
(global-set-key (kbd "<f10>") nil)
(global-set-key (kbd "<f11>") nil)
(global-set-key (kbd "<f12>") nil)

;>> general keybindings (end) ------------------------------------- >>
;<< -- personnal keybindings -------------------------------------- >>

(global-set-key (kbd "M-D") 'my:duplicate-line-or-region)
(global-set-key (kbd "M-*") 'my:toggle-case)

(global-set-key (kbd "<M-up>")   'my:go-up-and-toggle-comment-line)
(global-set-key (kbd "<M-down>") 'my:toggle-comment-line-and-go-down)

; personnal keybindings
(global-set-key (kbd "C-c t")   nil) ; make it a prefix
(global-set-key (kbd "C-c t m") 'menu-bar-mode)
(global-set-key (kbd "C-c t i") 'overwrite-mode)
(global-set-key (kbd "C-c t d") 'my:dark-theme)
(global-set-key (kbd "C-c t l") 'my:light-theme)
(global-set-key (kbd "C-x C-<left>")  'winner-undo)
(global-set-key (kbd "C-x C-<right>") 'winner-redo)

(global-set-key (kbd "<S-f4>")
  (lambda nil (interactive) (my:launch-terminal explicit-shell-file-name)))

(global-set-key (kbd "<backtab>")
  (read-kbd-macro "M-x i s w i t c h b - b u f f e r RET RET"))

(windmove-default-keybindings 'control)

;>> personnal keybindings (end) ----------------------------------- >>
;<< -- themes                -------------------------------------- >>

(defun my:dark-theme ()
  "Changes the theme to a dark one."
  (interactive)
  (load-theme 'acme-dark))

(defun my:light-theme ()
  "Changes the theme to a light one."
  (interactive)
  (load-theme 'acme-light))

;>> themes (end) -------------------------------------------------- >>
;<< -- compilation buffer    -------------------------------------- >>

(defvar cur nil) (defvar w nil) (defvar h nil)

(defun my:compile (&optional arg)
  "Run compile and resize the compile window"
  (interactive)
  (progn
    (if (and arg (stringp arg))
        (compile arg)
      (call-interactively 'compile))
    (setq cur (selected-window))
    (setq w (get-buffer-window "*compilation*"))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (- h 10))
    (select-window cur)))

(defun my-hooks:compilation-mode ()
  "Make sure that the compile window is splitting vertically"
  (if (not (get-buffer-window "*compilation*"))
      (split-window-vertically)))

(defun my:compile-check-delete (buf str)
  (interactive)
  (if (string= str "finished\n")
      (delete-window (get-buffer-window "*compilation*"))))

(add-hook 'compilation-mode-hook 'my-hooks:compilation-mode)
(add-to-list 'compilation-finish-functions 'my:compile-check-delete)

;>> compilation buffer (end) -------------------------------------- >>
;<< -- delimiters            -------------------------------------- >>

(defvar skeleton-pair-filter-function
  '(lambda ()
     (cond
      ((eq last-command-char ?\")
       (or (looking-at   (regexp-quote (string last-command-char)))
           (looking-back (regexp-quote (string last-command-char)))
           (looking-back "[[:graph:]]")))
      (t
       (looking-at (regexp-quote (string last-command-char)))))))

(ignore-errors (tm/initialize))

(when (require 'delim-kill nil t)
  (defun delim-kill-inside (from-char to-char orig-point save)
    "Kill the text between two characters, preserving balance.

Kills the text between the first occurence of FROM before point
and the first occurence of TO after point, excluding FROM and TO.

If FROM and TO are not identical, the function preserves the
balance between the two characters: For each FROM that is
encountered while looking for TO, one additional TO is required;
and vice versa. For example, in \"{ foo X{bar} baz }\", with X
being point and \"{\" and \"}\" as delimiters, the text \" foo
{bar} baz \" will be killed, not \"bar} baz \".

If FROM and TO are identical, and point is on that character when
the function is called, it ignores that single character and
moves to the next one, in both directions.

If beginning or end of buffer are reached, the function stops and
treats point-min resp. point-max as if the character had been
found there."
    (interactive "cFrom: \ncTo: \nd\nP")
      (let* ((from (delim-find-char-balanced-backward from-char to-char))
         (to   (delim-find-char-balanced-forward  from-char to-char)))
    (if (and from to)
        (delim-kill-it (+ from 1) (- to 1) save)
      (message "Not found!"))))

  (global-set-key (kbd "M-'") nil)     ; was `back-to-indentation'
  (global-set-key (kbd "M-' M-i") nil)
  (global-set-key (kbd "M-' M-a") nil)

  (defvar prefix)
  (defun my:set-keys (func from to)
    (eval `(global-set-key (kbd ,(concat prefix (char-to-string from)))
      (lambda (save) (interactive "P") (,func ,from ,to (point) save))))
    (eval `(global-set-key (kbd ,(concat prefix (char-to-string to)))
      (lambda (save) (interactive "P") (,func ,from ,to (point) save)))))

  (dolist (elt '(("M-' M-a " . delim-kill)
                 ("M-' M-i " . delim-kill-inside)))
    (setq prefix (car elt))
    (my:set-keys (cdr elt) ?\( ?\))
    (my:set-keys (cdr elt) ?{ ?})
    (my:set-keys (cdr elt) ?[ ?])
    (my:set-keys (cdr elt) ?\" ?\")
    (my:set-keys (cdr elt) ?« ?»)
    (my:set-keys (cdr elt) ?< ?>)
    (my:set-keys (cdr elt) ?' ?')))

;>> delimiters (end) ---------------------------------------------- >>
