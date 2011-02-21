;;; ps2pdf.el --- Convert Postscript to PDF

;; Copyright (C) 2005 Mathias Dahl

;; Version: 0.1.1
;; Keywords: PDF, PS, PostScript
;; Author: Mathias Dahl <mathias.removethis.dahl@gmail.com>
;; Maintainer: Mathias Dahl
;; URL: http://www.emacswiki.org/cgi-bin/wiki/PostScriptToPDF

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Just a small hack I did while playing with GhostScript, trying to
;; create PDF files from PostScript files.
;;
;; It does little more than the ps2pdf script that comes with
;; GhostScript.  Some handy functions to create PDF files from Emacs
;; buffers are provided though.
;;
;; If you use Windows you probably don't have GhostScript
;; installed.  Get it from here: http://www.cs.wisc.edu/~ghost/.  Make
;; sure you get one of the "GPL Ghostscript" versions.
;;
;; Tested under Windows XP and Mandriva GNU/Linux Free 2007.

;;; History:
;;
;; * Sat Dec 23 15:47:19 2006
;;
;;   Applied patch I got from Yasutaka SHINDOH <ring-pub at fan.gr.jp>
;;   The fix adds support for GNU/Linux and Unix-ish systems.
;;
;; TODO make the viewing optionnal (with a prefix for example or a variable)
;; TODO submit the changes

;;; Code:

(defcustom ps2pdf-gs-program "gs"
  "*Path to GhostView program.
For GNU/Linux and Unix-ish systems, this will typically just be
\"gs\", for w32, probably something like \"C:/Program
Files/GPLGS/gswin32c.exe\"."
  :type 'string
  :group 'ps2pdf)

(defcustom ps2pdf-gs-program-flags '("-q" "-dNOPAUSE" "-sDEVICE=pdfwrite" "-sPAPERSIZE=a4" "-sOutputFile=")
  "*A list of switches to pass to `ps2pdf-gs-program'.\n\
The last one directly precedes the output filename."
  :type '(repeat string)
  :group 'ps2pdf)

(defcustom ps2pdf-keep-ps-file nil
  "*If Non-nil, delete the ps file that served to produce the pdf."
  :type '(choice (const :tag "no" nil)
                 (const :tag "yes" t))
  :group 'ps2pdf)

(defcustom ps2pdf-black-and-white nil
  "*If Non-nil, produces black and white output instead."
  :type '(choice (const :tag "no" nil)
                 (const :tag "yes" t))
  :group 'ps2pdf)

(defcustom ps2pdf-open-program "xpdf"
  "*Path to PDF document viewer (not for w32).
For GNU/Linux and Unix-ish systems, this will be \"kpdf\" or
\"gpdf\" or similar."
  :type 'string
  :group 'ps2pdf)

(defun ps2pdf-convert (file)
  "Convert Postscript FILE to PDF."
  (setq file (expand-file-name file))
  (let* ((pdf-file (concat (file-name-sans-extension file) ".pdf"))
         (result
          (shell-command
           (format "%s %s%s %s" ps2pdf-gs-program
                   (combine-and-quote-strings ps2pdf-gs-program-flags) pdf-file file))))
    (if (eq 0 result)
      pdf-file
      (error "PDF creation failed"))))

;;;###autoload
(defun ps2pdf (file)
  "Convert Postscript FILE to PDF."
  (interactive "fSelect Postscript file: ")
  (let ((pdf-file (ps2pdf-convert file)))
    (message "PDF file %s created successfully" pdf-file)))

(defun ps2pdf-with-faces (type)
  "Create PDF from buffer or region."
  (let* ((dir (if (buffer-file-name) (expand-file-name (file-name-directory (buffer-file-name))) "/tmp/"))
        (psname (if (file-writable-p (substring dir 0 -1))
                  (concat dir (buffer-name) ".ps")
                  (make-temp-file "ps2pdf-buffer" nil ".ps")))
         pdfname)
    (cond ((and (eq type 'buffer) ps2pdf-black-and-white)
           (ps-print-buffer psname))
          ((and (eq type 'buffer))
           (ps-print-buffer-with-faces psname))
          ((and (eq type 'region) ps2pdf-black-and-white)
           (ps-print-region (region-beginning) (region-end) psname))
          ((eq type 'region)
           (ps-print-region-with-faces (region-beginning) (region-end) psname))
          (t (error "Type not supported")))
    (message "Postscript file %s created" psname)
    (setq pdfname (ps2pdf-convert psname))
    (if (not ps2pdf-keep-ps-file)
      (shell-command (format "rm %s" psname)))
    (message "PDF file %s created successfully" pdfname)
    pdfname))

;;;###autoload
(defun ps2pdf-from-region ()
  "Create PDF from region and open it."
  (interactive)
  (if (fboundp 'w32-shell-execute)
      (w32-shell-execute "Open" (ps2pdf-with-faces 'region))
    (shell-command
     (format "%s %s" ps2pdf-open-program (ps2pdf-with-faces 'region)))))

;;;###autoload
(defun ps2pdf-from-buffer ()
  "Create PDF from buffer and open it."
  (interactive)
  (if (fboundp 'w32-shell-execute)
      (w32-shell-execute "Open" (ps2pdf-with-faces 'buffer))
    (shell-command
     (format "%s %s" ps2pdf-open-program (ps2pdf-with-faces 'buffer)))))

(defun ps2pdf-reset-faces ()
  "Reset faces.
Use this if you change color theme or similar and the colors does
not look as you expect."
  (interactive)
  (setq ps-build-face-reference t))

(provide 'ps2pdf)

;;; ps2pdf.el ends here
