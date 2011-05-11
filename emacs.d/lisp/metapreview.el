;;; metapreview.el --- preview current MetaPost beginfig environment
;;;
;;; Copyright (C) 2009, 2010, 2011 René van Bevern
;;;
;;; Author: René van Bevern <rene.vanbevern@tu-berlin.de>
;;; URL: http://rvb.mytanet.de/preview-metapost-figures-from-emacs.shtml
;;;
;;; Usage: put file in some directory specified in the "load-path"
;;; variable (check with M-: load-path RET), then add (require
;;; 'metapreview) to your .emacs configuration file. Then, preview
;;; MetaPost figures using M-x meta-preview-fig RET. To bind the
;;; functionality to another key, say C-C, try something like
;;; 
;;;    (add-hook 'meta-mode-load-hook
;;;      '(lambda () (define-key meta-mode-map
;;;         (kbd "C-c C-c") 'meta-preview-fig)))
;;;
;;; Updated: 
;;;          2011-01-24: updated my E-mail address from Jena to Berlin
;;;                      Fixed MetaPost call.
;;;          2010-03-28: display output file as set in outputtemplate, 
;;;                      and filenametemplate in MetaPost input file
;;;          2010-03-27: to work with MetaPost 1.2
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above
;;;    copyright notice, this list of conditions and the following
;;;    disclaimer in the documentation and/or other materials provided
;;;    with the distribution.
;;; 3. The name of the author may not be used to endorse or promote
;;;    products derived from this software without specific prior
;;;    written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(require 'format-spec)
(require 'meta-mode)

(defun meta-fig-no ()
  "Return the figure number of the beginfig environment currently
containing the point."
  (save-excursion
    (next-line)
    (meta-beginning-of-defun)
    (re-search-forward "beginfig")
    (re-search-forward "(")
    (setq s (point))
    (re-search-forward ")")
    (buffer-substring s (1-(point)))))

(defun meta-filename-template ()
  "Return the set filename template preceding the beginfig
environment currently containing the point."
  (save-excursion
    (condition-case nil
	(progn (re-search-backward "outputtemplate\\|filenametemplate")
	       (search-forward "\"")
	       (setq s (point))
	       (search-forward "\"")
	       (buffer-substring s (1-(point))))
      (search-failed "%j.%c"))))

(defun meta-filename-template-replacements ()
  "List of filename template format strings with their
corresponding values."
  (let ((map '((?y . "%Y") (?m . "%m") (?d . "%d") (?H . "%H") (?M . "%M"))))
    (mapcar (lambda (x)
	      (cons (car x)
		    (string-to-number (format-time-string (cdr x)))))
	    map)))

(defun meta-fig-filename ()
  "The prospective output filename to which MetaPost will render
the beginfig environment that currently contains the point."
  (let ((job (file-name-sans-extension
	      (file-name-nondirectory buffer-file-name)))
	(no (meta-fig-no))
	(template (meta-filename-template)))
    (format-spec template (append (list (cons ?j job) (cons ?c no))
				  (meta-filename-template-replacements)))))

(defun meta-preview-fig ()
  "Preview the beginfig environment currently containing the
point. Runs the currently open file through MetaPost. Opens
*Metapost* buffer if something goes wrong."
  (interactive)
  (if (buffer-modified-p) (save-buffer))
  (let ((base (file-name-nondirectory buffer-file-name)))
    (if (= 0 (call-process-shell-command
	      (concat "mpost '\\prologues:=3; input "
		      base "'") nil "*Metapost*"))
      (call-process-shell-command
       (concat "gv " (shell-quote-argument (meta-fig-filename))) nil 0)
      (view-buffer "*Metapost*"))))

(provide 'metapreview)
