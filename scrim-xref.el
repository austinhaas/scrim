;;; scrim-xref.el --- xref support for scrim       -*- lexical-binding: t; -*-

;; Copyright © 2023 Austin Haas
;;
;; Author: Austin Haas <austin@pettomato.com>
;; URL: http://github.com/austinhaas/scrim

;; This file is not part of GNU Emacs.

;;; Commentary:

;; xref support for scrim.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Dependencies that are included with Emacs

(require 'arc-mode)
(require 'cl-lib)
(require 'subr-x)
(require 'xref)

;; Additional dependencies

(require 'clojure-mode)
(require 'scrim)

(defun scrim--xref-backend () 'scrim)

(defun scrim--archive-extract (archive file)
  "Extract archive into its own buffer and return the buffer. Does
not create a new buffer if one already exists."
  ;; Implementation based on `archive-extract'.
  (let* ((arcdir (file-name-directory archive))
         (arcname (file-name-nondirectory archive))
         (bufname (concat (file-name-nondirectory file) " (" arcname ")"))
         (read-only-p t)
         ;; `archive-extract' uses arcname here instead of archive, because it
         ;; assumes `default-directory' is the archive, and `expand-file-name'
         ;; will use that to expand the relative path.
         (arcfilename (expand-file-name (concat archive ":" file)))
         (buffer (get-buffer bufname)))
    (if (and buffer
             (string= (buffer-file-name buffer) arcfilename))
        buffer
      (setq bufname (generate-new-buffer-name bufname))
      (setq buffer (get-buffer-create bufname))
      (with-current-buffer buffer
        (let ((coding-system-for-read 'prefer-utf-8))
          (archive-zip-extract archive file))
        (setq buffer-file-name arcfilename)
        (setq buffer-file-truename (abbreviate-file-name buffer-file-name))
        (setq default-directory arcdir)
        (add-hook 'write-file-functions #'archive-write-file-member nil t)
        (archive-set-buffer-as-visiting-file file)
        (setq buffer-read-only t)
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil)
        (setq buffer-saved-size (buffer-size))
        (normal-mode)
        (run-hooks 'archive-extract-hook))
      (archive-maybe-update t)
      buffer)))

(defclass scrim--xref-archive-location ()
  ((archive :type string :initarg :archive)
   (file :type string :initarg :file)
   (line :type fixnum :initarg :line)
   (column :type fixnum :initarg :column))
  :documentation "An archive location is an archive/file/line/column quadruple.
Line numbers start from 1 and columns from 0.")

(defun scrim--xref-make-archive-location (archive file line column)
  "Create and return a new `scrim--xref-archive-location'."
  (make-instance 'scrim--xref-archive-location
                 :archive archive
                 :file file
                 :line line
                 :column column))

(cl-defmethod xref-location-marker ((l scrim--xref-archive-location))
  ;; Implementation based on `xref-location-marker' implementation for
  ;; xref-file-location.
  (with-slots (archive file line column) l
    (with-current-buffer (scrim--archive-extract archive file)
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (ignore-errors
            ;; xref location may be out of date; it may be past the end of the
            ;; current file, or the file may have been deleted. Return a
            ;; reasonable location; the user will figure it out.
            (beginning-of-line line)
            (forward-char column))
          (point-marker))))))

(cl-defmethod xref-location-group ((l scrim--xref-archive-location))
  (with-slots (archive file) l
    (concat (file-name-nondirectory file) " (" archive ")")))

(defun scrim--get-xref (symbol file line column)
  "Retuns an xref for the given arguments. If the parameters
specify a location in a jar or zip file, it will attempt to
extract the file from the archive and load it into a buffer
before returning an xref."
  (cond ((string-match "^file:\\(.*\\)" file)
         (let ((file (match-string 1 file)))
           (xref-make (prin1-to-string symbol)
                      (xref-make-file-location file line column))))
        ((string-match "^\\(jar\\|zip\\):file:\\(.+\\)!/\\(.+\\)" file)
         (let* ((archive (match-string 2 file))
                (file (match-string 3 file)))
           (xref-make (prin1-to-string symbol)
                      (scrim--xref-make-archive-location archive file line column))))))

;;; Backend implementation

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql scrim)))
  (if-let ((symbol (scrim-symbol-at-point)))
      (scrim--repl-get-namespaced-symbol symbol)
    (user-error "No symbol at point.")))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql scrim)))
  (scrim--repl-get-all-namespaced-symbols))

(cl-defmethod xref-backend-definitions ((_backend (eql scrim)) identifier)
  ;; TODO: Return all generic method implementations for
  ;; identifier. The tricky thing about this is that we are currently
  ;; relying on var metadata to give us the source location, and that
  ;; only includes the defmulti. Maybe we'd be better off with an
  ;; alternative approach for all cases, since the current method
  ;; can't handle definitions evaluated at run-time, either, which is
  ;; annoying.

  ;; TODO: Determine how to handle vars that were evaluated in the REPL. If we
  ;; use this bogus location xref, an error will be thrown if any of these are
  ;; returned. If we use nil, the var will be silently elided. Maybe we should
  ;; return a buffer xref to the REPL?

  ;; (xref-make-bogus-location "NO_SOURCE_PATH")

  (pcase (scrim--repl-get-path-to-symbol-source identifier)
    (`(,file ,line ,column) (when-let ((xref (scrim--get-xref identifier file line column)))
                              (list xref)))))

(cl-defmethod xref-backend-references ((_backend (eql scrim)) identifier)
  ;; TODO: Implement support for finding references in jars.
  (or (mapcan (lambda (x)
                (let* ((file (car x))
                       (file (when (string-prefix-p "file:" file)
                               (string-remove-prefix "file:" file)))
                       (strings (cadr x))
                       ;; Emacs's special constructs, like `\_<`, can't be used
                       ;; here because `xref-matches-in-files` uses grep.
                       (regexp (concat "[[:space:](]" (regexp-opt strings t) "[[:space:])]")))
                  (when file
                    (xref-matches-in-files regexp (list file)))))
              (scrim--repl-get-possible-references identifier))
      (cl-call-next-method)))

(cl-defmethod xref-backend-apropos ((backend (eql scrim)) pattern)
  (mapcan (lambda (symbol) (xref-backend-definitions backend symbol))
          (read (scrim--redirect-result-from-process
                 (scrim-proc)
                 (format "(clojure.repl/apropos \"%s\")"
                         pattern)))))
