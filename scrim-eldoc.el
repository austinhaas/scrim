;;; scrim-eldoc.el --- Eldoc support for scrim.

;; Copyright © 2021 Austin Haas
;;
;; Author: Austin Haas <austin@pettomato.com>
;; URL: http://github.com/austinhaas/scrim
;; Version: 0.0.4
;; Package-Requires: ((scrim "0.0.4"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; (require 'scrim-eldoc)

;; See README.md.

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

(require 'scrim)

(defvar-local scrim--eldoc-cache nil)

(defun scrim--get-arglists (sym)
  (let ((result (scrim-redirect-result-from-process
                 (scrim-proc)
                 (format "(:arglists (meta (resolve '%s)))"
                         sym))))
    (if (string-equal result "nil")
        nil
      result)))

(defun scrim--get-docstring (sym)
  (scrim-redirect-result-from-process
   (scrim-proc)
   (format "(clojure.repl/doc %s)"
           sym)))

(defun scrim--get-ns ()
  (read
   (scrim-redirect-result-from-process
    (scrim-proc)
    "(str *ns*)")))

(defun scrim--get-special-form-signature (sym)
  (when-let ((doc   (scrim--get-docstring sym))
             (lines (split-string doc "[\n]" nil "[ ]+")))
    (caddr lines)))

(defun scrim-mode-eldoc-function ()
  (when (scrim-proc)
    (let ((buffer-ns (clojure-find-ns))
          (actual-ns (scrim--get-ns)))
      ;;(message "ns: buffer: %s actual: %s" buffer-ns actual-ns)
      (when (or (string-equal actual-ns "") ;; *ns* is always nil in cljs, so skip this check.
                (string-equal buffer-ns actual-ns))
        (when (not (nth 4 (syntax-ppss))) ; inside a comment?
          (when-let ((sym (or (scrim-current-function-symbol)
                              (scrim-symbol-at-point))))
            (if (get-buffer-process scrim--buffer-name)
                (cond
                 ((string= sym (car scrim--eldoc-cache)) (cdr scrim--eldoc-cache))
                 ((string-prefix-p ":" sym) nil)
                 ((string-prefix-p "#_" sym) nil)
                 ((string-suffix-p "/" sym) nil)
                 ((string-suffix-p "#" sym) nil)
                 ((string-suffix-p ".-" sym) nil)
                 (t (let* ((result (or (scrim--get-arglists sym)
                                       (scrim--get-special-form-signature sym)
                                       "<unknown symbol>"))
                           (s      (format "%s: %s"
                                           (propertize sym 'face 'font-lock-function-name-face)
                                           result)))
                      (setq scrim--eldoc-cache (cons sym s))
                      s)))
              (let ((s (format "%s: %s"
                               (propertize sym 'face 'font-lock-function-name-face)
                               "<not connected>")))
                (setq scrim--eldoc-cache (cons sym s))
                s))))))))

(add-hook 'clojure-mode-hook 'eldoc-mode)

(add-hook 'scrim-minor-mode-hook
          (lambda ()
            (add-function :before-until
                          (local 'eldoc-documentation-function)
                          'scrim-mode-eldoc-function)))

(add-hook 'scrim-minor-mode-hook 'eldoc-mode)

(provide 'scrim-eldoc)

;;; scrim-eldoc.el ends here
