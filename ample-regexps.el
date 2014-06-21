;;; ample-regexps.el --- ample regular expressions for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2014 immerrr

;; Author: immerrr <immerrr@gmail.com>
;; Created: 22 Jun 2014
;; Version: 0.1
;; Keywords: regexps, extensions, tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Compose and reuse regular expressions with ease.
;;
;; If you ever tried to write more than a few of related regexps and it felt
;; that there should be a way to pick out their common parts and just plug them
;; in without worrying about grouping and precedence, this package is for you.
;;
;; It is implemented over the great Emacs package called `rx' that enables
;; structural regexp editing while avoiding the colorful jungles of backslashes
;; and spices it up with the ability to define your own subforms and use them
;; to construct readable and maintainable regexps.

;;; Code:

(require 'rx)

;; Make sure `rx-parent' is dynamically bound
(defvar rx-parent)

(defun arx--ensure-regexp (maybe-regexp)
  "Convert rx form to regexp string if necessary."
  (if (listp maybe-regexp)
      (rx-form maybe-regexp rx-parent)
    maybe-regexp))

(defun arx--quoted-literal (literal &optional form)
  (rx-check (list form))
  (rx-form literal rx-parent))


(defun arx--apply-form-func (form-func form)
  "Apply function to form, return result as regexp string."
  (rx-check form)
  (arx--ensure-regexp (apply form-func form)))


(defun arx--alias-rx-form (rx-form form)
  (rx-check (list form))
  (arx--ensure-regexp rx-form))


(defun arx--to-rx (form)
  (unless (listp form)
    (error "arx: Form is not a list: %S" form))

  (let* ((form-name (car form))
         (form-defn (cadr form)))
    (cons form-name
          (cond
           ((listp form-defn)
            (if (eq (car-safe form-defn) :func)
                ;; fancy function definition
                (append
                 (list (apply-partially #'arx--apply-form-func
                                        (plist-get form-defn :func)))
                 (mapcar (lambda (kwarg)
                           (plist-get form-defn kwarg))
                         '(:min-args :max-args :predicate)))
              ;; This doesn't work:
              ;;
              ;;     (list (lambda (form) (arx--alias-rx-form form-defn form))
              ;;           0 0)
              ;;
              ;; because of
              ;;
              ;;     Lisp error: (void-function closure)
              ;;
              ;; Why?
              (list (apply-partially #'arx--alias-rx-form form-defn) 0 0)))
           ((stringp form-defn)
            (list (apply-partially #'arx--quoted-literal form-defn) 0 0))

           ((symbolp form-defn)
            ;; already a valid rx form, do nothing
            form-defn)

           (t (error "arx: Incorrect form: %S" form))))))

;;;###autoload
(defmacro define-arx (macro &rest forms)
  (let* ((macro-name (symbol-name macro))
         (macro-to-string (intern (concat macro-name "-to-string")))
         (macro-constituents (intern (concat macro-name "-constituents"))))
    `(progn
       (defvar ,macro-constituents)
       (setq ,macro-constituents (copy-sequence rx-constituents))
       (mapc (lambda (form)
               (push (arx--to-rx form) ,macro-constituents))
             (quote ,forms))

       (defun ,macro-to-string (form &optional no-group)
         (let ((rx-constituents ,macro-constituents))
           (rx-to-string form no-group)))

       (defmacro ,macro (&rest regexps)
         "Translate regular expressions REGEXPS in sexp form to a regexp string.

This is an extension of the standard `rx' macro via the
`define-arx' functionality."
         (cond ((null regexps)
                (error "No regexp"))
               ((cdr regexps)
                (,macro-to-string `(and ,@regexps) t))
               (t
                (,macro-to-string (car regexps) t)))))))

;;;###autoload
(defun arx-and (forms)
  "Generate an expression to match a sequence of FORMS."
  (let ((rx-parent (if (boundp 'rx-parent) rx-parent nil)))
    (if (null forms)
        ""
      (rx-and `(seq ,@forms)))))

;;;###autoload
(defun arx-or (forms)
  "Generate an expression to match one of FORMS."
  (let ((rx-parent (if (boundp 'rx-parent) rx-parent nil)))
   (if (> (length forms) 1)
       (rx-or `(or ,@forms))
     (arx-and forms))))

(provide 'ample-regexps)

;;; ample-regexps.el ends here
