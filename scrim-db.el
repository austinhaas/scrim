;;; scrim-db.el --- Cache info about namespaces and symbols       -*- lexical-binding: t; -*-

;; Copyright © 2021 Austin Haas
;;
;; Author: Austin Haas <austin@pettomato.com>
;; URL: http://github.com/austinhaas/scrim

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The idea is to create an offline database of Clojure symbol metadata that can
;; be used to support features like find-definition and eldoc.

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

(require 'cl-lib)
(require 'subr-x)
(require 'xref)

;; Additional dependencies

(require 'clojure-mode)
(require 'scrim)

(defvar scrim--db nil)

(defvar scrim--db-filename ".scrim-db")

(defun scrim--db-file-path ()
  (concat (scrim-project-root) scrim--db-filename))

;; Doesn't work in cljs, because cljs doesn't have `all-ns`.
(defvar scrim--build-db-clj
  ;; This produces a clojure value that can be read by emacs lisp.
  "(letfn [(alist [xs] (for [[k v] xs] (list k '. v)))
        #_(alist [xs] (into {} xs))]
  (alist
   (concat
    [[\"#special-forms#\"
      (alist
       (for [[k v] (deref #'clojure.repl/special-doc-map)]
         (if-let [forms (get v :forms)]
           (if (every? (fn [form] (= k (first form))) forms)
             (let [arglists (map (comp vec rest) forms)]
               [(str k) (alist {\"forms\"    (str forms)
                                \"arglists\" (pr-str arglists)})])
             [(str k) (alist {\"forms\" (str forms)})])
           [(str k) nil])))]]
    (for [ns (all-ns)]
      [(str (ns-name ns))
       (alist
        {\"aliases\"
         (alist
          (for [[k v] (ns-aliases ns)]
            [(name k) (name (ns-name v))]))

         \"refers\"
         (alist
          (for [[k v] (ns-refers ns)
                :let  [ns-name (ns-name (:ns (meta v)))]
                ;; To save space.
                :when (not= 'clojure.core ns-name)]
            [(name k) (name ns-name)]))

         \"interns\"
         (alist
          (for [[k v] (ns-interns ns)]
            [(name k) (alist
                       (let [m (meta v)]
                         {\"arglists\" (some-> (get m :arglists) str )
                          \"file\"     (when-let [x (get m :file)]
                                       (case x
                                         \"NO_SOURCE_PATH\" x
                                         (str (.getResource
                                               (clojure.lang.RT/baseLoader)
                                               x))))
                          \"column\"   (get m :column)
                          \"line\"     (get m :line)
                          \"name\"     (name (get m :name))
                          \"ns\"       (name (ns-name (get m :ns)))}))]))})]))))")

(defun scrim-build-db ()
  "Send a clojure expression to the REPL to create the database
based on the namespaces that are currently loaded."
  (interactive nil scrim-mode scrim-minor-mode)
  (if (get-buffer scrim--buffer-name)
      (let ((pr (make-progress-reporter "Building database"))
            (db (scrim-redirect-result-from-process (scrim-proc)
                                                    scrim--build-db-clj)))
        (setq scrim--db (read db))
        (progress-reporter-done pr))
    (user-error "Not connected.")))

(defun scrim-save-db ()
  (interactive nil scrim-mode scrim-minor-mode)
  (message "Saving scrim db...")
  (with-temp-buffer
    (print scrim--db (current-buffer))
    (write-file (scrim--db-file-path)))
  (message "Saving scrim db...done"))

(defun scrim-load-db ()
  (interactive nil scrim-mode scrim-minor-mode)
  (message "Loading scrim db...")
  (let ((f (scrim--db-file-path)))
    (if (file-exists-p f)
        (with-temp-buffer
          (insert-file-contents f)
          (setq scrim--db (read (current-buffer)))
          (message "Loading scrim db...done"))
      (message "Could not find scrim db file."))))

(defun scrim--get (alist key &optional default)
  "Finds a value in an alist, where the keys are strings. Optional
default value will be returned if key is not found."
  (if (listp alist)
      (alist-get key alist default nil #'string-equal)
    default))

(defun scrim--get-in (alist keys &optional default)
  "Finds a value in a nested alist, where all keys are strings. Optional
default value will be returned if any keys are not found."
  (if keys
      (let* ((sentinel (gensym))
             (result (scrim--get alist (car keys) sentinel)))
        (if (eq result sentinel)
            default
          (scrim--get-in result (cdr keys) default)))
    alist))

(defvar scrim--symbol-regexp "^\\(.*\\)/\\(.*\\)"
  "A very permissive regexp that can be used to split a string
naming a Clojure symbol into namespace and symbol-name
components. It shouldn't be used to validate symbols.")

(defun scrim--parse-symbol (symbol)
  "Parse a Clojure symbol into namespace and symbol-name
components. Returns (symbol-ns . symbol-name). symbol-ns may be a
fully-qualified namespace, nil, or an alias."
  (if (string-match scrim--symbol-regexp symbol)
      (cons (match-string 1 symbol)
            (match-string 2 symbol))
    (cons nil symbol)))

(defun scrim--db-lookup-symbol (current-ns symbol)
  "Returns the metadata alist associated with symbol in
scrim--db. The symbol is looked up in the context of
current-ns. For example, if symbol is str/trim, where str is an
alias for clojure.string in current-ns, then the metadata
associated with clojure.string/trim will be returned.

Both args are strings."
  (let* ((parsed-symbol (scrim--parse-symbol symbol))
         (symbol-ns-local (car parsed-symbol))
         (symbol-name (cdr parsed-symbol)))
    (if (null symbol-ns-local)
        ;; Symbol does not have a namespace component.
        (or
         ;; Symbol was refer'd.
         (when-let ((symbol-ns (scrim--get-in scrim--db (list current-ns "refers" symbol-name))))
           (scrim--get-in scrim--db (list symbol-ns "interns" symbol-name)))
         ;; Symbol is in the current ns.
         (scrim--get-in scrim--db (list current-ns "interns" symbol-name))
         ;; Symbol is in clojure.core. scrim--build-db-clj elides symbols in
         ;; clojure.core to save space, so we need to check for it explicitly.
         (scrim--get-in scrim--db (list "clojure.core" "interns" symbol-name))
         ;; Symbol is a special-form.
         (scrim--get-in scrim--db (list "#special-forms#" symbol-name)))
      ;; Symbol has a namespace component.
      (or
       ;; Alias
       (when-let ((symbol-ns (scrim--get-in scrim--db (list current-ns "aliases" symbol-ns-local))))
         (scrim--get-in scrim--db (list symbol-ns "interns" symbol-name)))
       ;; Fully-qualified
       (scrim--get-in scrim--db (list symbol-ns-local "interns" symbol-name))))))

(defun scrim--db-get-every-namespaced-symbol ()
  (mapcan (lambda (ns)
            (mapcar (lambda (sym) (concat (car ns) "/" (car sym)))
                    (scrim--get ns "interns")))
          scrim--db))

(defun scrim--db-get-symbol-metadata (ns symbol)
  (scrim--db-lookup-symbol ns symbol))

(defun scrim--db-get-apropos-locations (pattern)
  (let ((regexp (xref-apropos-regexp pattern)))
    (seq-filter (lambda (x) (string-match regexp (car x)))
                (seq-remove #'null
                            (apply #'append
                                   (mapcar (lambda (ns) (scrim--get ns "interns"))
                                           scrim--db))))))

(defun scrim--db-find-definition-location (identifier)
  (let* ((ns (clojure-find-ns))
         (alist (scrim--db-get-symbol-metadata ns identifier))
         (file (scrim--get alist "file"))
         (line (scrim--get alist "line"))
         (column (scrim--get alist "column")))
    (when (and file (not (string-equal file "NO_SOURCE_PATH")))
      (list file line column))))

(defun scrim--db-find-possible-references (identifier)
  "Takes a namespaced symbol and returns a list of (file strings)."
  ;; This has some limitations:

  ;;   `identifier` must be a fully qualified symbol.

  ;;   Doesn't detect references via :use.

  ;;   Doesn't find fully qualified symbols, unless they are also mentioned in
  ;;   the namespace's refers or aliases.

  ;;   Doesn't find symbols evaluated in the REPL.
  (let* ((parsed-symbol (scrim--parse-symbol identifier))
         (symbol-ns (car parsed-symbol))
         (symbol-name (cdr parsed-symbol)))
    (mapcar (lambda (ns)
              ;; HACK! Getting the file from the ns based on the first public
              ;; symbol in that ns.
              (let* ((file (seq-some (lambda (file)
                                       (if (string-prefix-p "file:" file)
                                           file
                                         nil))
                                     (mapcar
                                      (lambda (x) (scrim--get x "file"))
                                      (scrim--get ns "interns"))))
                     (refers (or (string-equal (scrim--get-in ns (list "refers" symbol-name)) symbol-ns)
                                 (and (string-equal "clojure.core" symbol-ns)
                                      (scrim--get-in scrim--db (list "clojure.core" "interns" symbol-name)))))
                     (alias (seq-some (lambda (x)
                                        (and (string-equal (cdr x) symbol-ns)
                                             (car x)))
                                      (scrim--get ns "aliases")))
                     (strings (seq-remove #'null
                                          (list (and refers symbol-name)
                                                (and alias (concat alias "/" symbol-name))))))
                (when (and file strings)
                  (list file strings))))
            scrim--db)))

(defun scrim--db-completion-table (s)
  (let* ((ns (clojure-find-ns))
         (ns-alist (scrim--get scrim--db ns))
         (interns (scrim--get ns-alist "interns"))
         (refers (scrim--get ns-alist "refers"))
         (aliases (scrim--get ns-alist "aliases")))
    (nconc
     (mapcar #'car (scrim--get-in scrim--db (list "#special-forms#")))
     (mapcar #'car (scrim--get-in scrim--db (list "clojure.core" "interns")))
     (mapcar #'car interns)
     (mapcar #'car refers)
     (mapcan (lambda (x)
               (let* ((alias (car x))
                      (alias-ns (cdr x))
                      (alias-interns (scrim--get-in scrim--db (list alias-ns "interns"))))
                 (mapcar (lambda (x) (concat alias "/" (car x)))
                         alias-interns)))
             aliases))))

(defun scrim--db-eldoc-function ()
  (when (not (nth 4 (syntax-ppss)))    ; inside a comment?
    (when-let ((sym (or (scrim-current-function-symbol)
                        (scrim-symbol-at-point))))
      (let* ((ns (clojure-find-ns))
             (alist (scrim--db-lookup-symbol ns sym)))
        (when alist
          (if-let ((arglist (scrim--get alist "arglists")))
              (format "%s: %s"
                      (propertize sym 'face 'font-lock-function-name-face)
                      arglist)
            (if-let ((forms (scrim--get alist "forms")))
                (format "%s: %s"
                        (propertize sym 'face 'font-lock-function-name-face)
                        forms)
              (format "%s"
                      (propertize sym 'face 'font-lock-function-name-face)))))))))
