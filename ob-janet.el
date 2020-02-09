;;; ob-janet.el --- Janet language support in Emacs Org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 DEADB17

;; Author: DEADB17
;; Version: 1.0.0
;; Created: 2020-02-09
;; Keywords: literate programming, janet
;; Homepage: https://github.com/DEADB17/ob-janet

;; This file is not part of GNU Emacs

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Support for evaluating janet code in org-mode
;; See https://orgmode.org/manual/Working-with-source-code.html

;; Requirements:

;; - Janet, see http://janet-lang.org/
;; - janet-mode https://github.com/ALSchwalm/janet-mode

;;; Code:

(require 'ob)

;; add janet to languages supported by org
(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("janet" . "janet"))

(defcustom org-babel-janet-hline-to "nil"
  "Replace hlines in incoming tables with this when translating to janet."
  :group 'org-babel
  :version "27.0.50"
  :package-version '(Org . "9.3.4")
  :type 'string)

(defcustom org-babel-janet-nil-to 'hline
  "Replace 'nil' in janet tables with this before returning."
  :group 'org-babel
  :version "27.0.50"
  :package-version '(Org . "9.3.4")
  :type 'symbol)

(defvar org-babel-default-header-args:janet
  '((:cmd . "janet"))
  "Default arguments when evaluating a Janet source block.
Defaulting `:cmd' to `janet'.")

(defvar org-babel-function-wrapper:janet
  "(pp (do %s))"
  "Janet code to print value of body.")

(defun ob-janet--table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an Emacs-lisp table,
otherwise return the results as a string."
  (let ((res (org-babel-script-escape (string-trim results))))
    (if (listp res)
        (mapcar
         (lambda (el)
           (if (equal el 'nil)
               org-babel-janet-nil-to el))
         res)
      res)))

(defun ob-janet--vars-to-values (vars)
  "Convers VARS to a string of janet code.
VARS are wrapped with def."
  (mapcar (lambda (var)
            (concat
             "(def"
             (format " %s " (car var))
             (format (if (listp (cdr var)) "'%S" "%S") (cdr var))
             ")"))
          vars))

(defun ob-janet--expand-fmt (fmt &optional params)
  "Expands a format list `FMT', and return a string.
PARAMS
Substitutes symbols according to the `params` alist.
The `fmt` argument may also be a string, in which
case it is returned as is."
  (if (stringp fmt)
      fmt
    (mapconcat
     (lambda (x)
       (cond
        ((stringp x) x)
        ((eq x 'ln) "\n")
        ((eq x 'quot) "\"")
        ((eq x 'apos) "\'")
        ((symbolp x)
         (let ((p (cdr (assq x params))))
           (unless p
             (error "Key %s not in %S" x params))
           (format "%s" p)))
        (t (error "Expected string or symbol: %S" fmt))))
     fmt "")))

(defun ob-janet--wrap-body (body vars prologue epilogue)
  "Wraps BODY VARS, PROLOGUE and EPILOGUE if present.
Returns the wrapped body as a string."
  (let ((var-defs nil))
    (when (> (length vars) 0)
      (setq var-defs (ob-janet--vars-to-values vars)))
    (mapconcat #'identity
               (append
                (when prologue (list (ob-janet--expand-fmt pro)))
                var-defs
                (list body)
                (when epilogue (list (ob-janet--expand-fmt epi))))
               "\n")))

(defun org-babel-execute:janet (body params)
  "Evaluate a `janet' code block.  BODY and PARAMS.

Some custom header arguments are supported to control the
evaluation.  These are:

- :cmd which allows to set the Janet executable and the switches
  on each code block.

- :debug which outputs the body before passing it to the
  interpreter.

- :eval-file FILENAME which writes the body to FILENAME and then
  evaluates the result.  When FILENAME is equal to \"\" it is
  derived from the code-block name."
  (let ((vars        (org-babel--get-vars params))
        (prologue    (alist-get :prologue params))
        (epilogue    (alist-get :epilogue params))
        (cmd         (alist-get :cmd params "janet"))
        (result-type (alist-get :result-type params))
        (ext         (alist-get :file-ext params "janet"))
        (file        (alist-get :file params))
        (eval-file   (alist-get :eval-file params))
        x-body)

    (when (eq "" eval-file)
      (setq eval-file (alist-get :file
                                 (org-babel-generate-file-param
                                  (nth 4 (org-babel-get-src-block-info))
                                  (cons (cons :file-ext ext) params)))))

    (setq x-body (if (or vars prologue epilogue)
                     (ob-janet--wrap-body body vars prologue epilogue)
                   body))

    (when (and (not file)
               (not eval-file)
               (string= result-type "value"))
      (setq x-body (format org-babel-function-wrapper:janet x-body)))

    (if (assq :debug params)
        x-body
      (if file
          (with-temp-file file (insert x-body))
        (let* ((temp (or eval-file
                         (org-babel-temp-file "ob-" (concat "." ext))))
               (result (progn (with-temp-file temp (insert x-body))
                              (org-babel-eval (concat cmd " " temp) ""))))
          (org-babel-reassemble-table
           (org-babel-result-cond (alist-get :result-params params)
             result
             (ob-janet--table-or-string result))
           (org-babel-pick-name (alist-get :colname-names params)
                                (alist-get :colnames params))
           (org-babel-pick-name (alist-get :rowname-names params)
                                (alist-get :rownames params))))))))

(defun org-babel-prep-session:janet (session params)
  "Not implemented.  SESSION and PARAMS are discarded."
  (error "`janet` presently does not support sessions"))

(provide 'ob-janet)

;;; ob-janet.el ends here
