;;; ample-regexps.el --- ample regular expressions for Emacs

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

;; Compose and reuse Emacs regular expressions with ease.
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
(require 'help-fns)

;; Make sure `rx-parent' is dynamically bound
(defvar rx-parent)

(defun arx--ensure-regexp (maybe-regexp)
  "Convert MAYBE-REGEXP to string if it is an rx form."
  (if (listp maybe-regexp)
      (rx-form maybe-regexp rx-parent)
    maybe-regexp))

(defun arx--quoted-literal (literal &optional form)
  "Regexp-quote and shy-group LITERAL as necessary.

When partially applied, can be added to `rx' constituents to
handle FORM."
  (unless (listp form)
    (setq form (list form)))
  (rx-check form)
  (rx-form literal rx-parent))


(defun arx--apply-form-func (form-func form)
  "Apply FORM-FUNC to FORM, return result as regexp string.

When partially applied, can be added to `rx' constituents to
handle FORM."
  (rx-check form)
  (arx--ensure-regexp (apply form-func form)))


(defun arx--alias-rx-form (aliased-form form)
  "Convert ALIASED-FORM to string.

When partially applied, can be added to `rx' constituents to
handle FORM."
  (rx-check (list form))
  (rx-form aliased-form rx-parent))


(defun arx--bound-interval (interval lower upper)
  "Restrict INTERVAL with LOWER and UPPER boundaries.

INTERVAL is a list: (MIN MAX).
LOWER and UPPER may be nil, which means 'no boundary'.

Returned value is a list (MIN-MAYBE MAX-MAYBE), where MIN-MAYBE
is non-nil only if greater than 0 and MAX-MAYBE is non-nil only
if less than `most-positive-fixnum'."
  (let ((i-min (car interval))
        (i-max (cadr interval)))
    (when lower
      (setq i-min (max i-min lower)))
    (when upper
      (setq i-max (min i-max upper)))

    (list (when (< 0 i-min) i-min)
          (when (< i-max most-positive-fixnum) i-max))))


(defun arx--function-arity (func)
  "Get min and max number of arguments accepted by FUNC."
  (let ((arglist (help-function-arglist func))
        (min-args 0) max-args)
    ;; Count required arguments.
    (while (and arglist
                (not (memq (car arglist) '(&rest &optional))))
      (setq min-args (1+ min-args))
      (setq arglist (cdr arglist)))

    ;; Count optional arguments.
    (setq max-args min-args)
    (when (eq (car-safe arglist) '&optional)
      (setq arglist (cdr arglist))
      (while (and arglist
                  (not (eq (car arglist) '&rest)))
        (setq max-args (1+ max-args))
        (setq arglist (cdr arglist))))

    ;; If rest is present, assign max-args maxint.
    (list (1- min-args)
          (if (eq (car-safe arglist) '&rest)
              most-positive-fixnum
            (1- max-args)))))


(defun arx--to-rx (arx-form)
  "Convert ARX-FORM to rx format.

ARX-FORM must be list containing one element according to the
`define-arx' documentation."

  (unless (listp arx-form)
    (error "Form is not a list: %S" arx-form))

  (let* ((form-name (car arx-form))
         (form-defn (cadr arx-form)))
    (cons form-name
          (cond
           ((listp form-defn)
            (if (eq (car-safe form-defn) :func)
                (let* ((func (if (functionp (plist-get form-defn :func))
                                 (byte-compile (plist-get form-defn :func))
                               (error "Not a function: %S" (plist-get form-defn :func))))
                       (min-args (plist-get form-defn :min-args))
                       (max-args (plist-get form-defn :max-args))
                       (arity (arx--bound-interval (arx--function-arity func)
                                                   min-args max-args))
                       (predicate (plist-get form-defn :predicate)))
                  ;; fancy function definition
                  `( ,(apply-partially #'arx--apply-form-func func)
                     ,@arity ,predicate))
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
            (list (apply-partially #'arx--quoted-literal form-defn)
                  0 0 nil))

           ((symbolp form-defn)
            ;; already a valid rx form, do nothing
            form-defn)

           (t (error "Incorrect arx-form: %S" arx-form))))))

(defun arx--form-make-docstring (arx-form)
  "Make docstring for given ARX-FORM."
  (let ((form-sym (car arx-form))
        (form-defn (cadr arx-form))
        header docstring)
    (cond
     ((symbolp form-defn)
      (setq header form-sym
            docstring (format "An alias for `%s'." form-defn)))
     ((stringp form-defn)
      (setq header form-sym
            docstring (format "A pre-rendered regexp: %S." form-defn)))
     ((eq :func (car-safe form-defn))
      (let* ((func (plist-get form-defn :func))
             (arglist (help-function-arglist func)))
        (setcar arglist form-sym)
        (setq header arglist
              docstring (or (documentation func)
                            "Function without documentation."))))
     ((listp form-defn)
      (setq header form-sym
            docstring (format "An alias for `%s'." form-defn))))
    (format "`%s'\n%s" header
            (with-temp-buffer
              (insert docstring)
              (indent-rigidly (point-min) (point-max) 4)
              (buffer-substring-no-properties (point-min) (point-max))))))


(defun arx--fnsym-in-current-sexp ()
  "Return current function symbol and current arg index."
  (save-excursion
    (when (nth 8 (syntax-ppss))
      ;; Exit innermost string or comment.
      (goto-char (nth 8 (syntax-ppss))))
    (elisp--fnsym-in-current-sexp)))


(defun arx--name-and-depth ()
  "Return name of innermost arx function and relative depth to it."
  (save-excursion
    (let (beginning-of-innermost-sexp
          fnword
          fnsym
          found-arx-form
          (depth 0))
      (while (and (not found-arx-form)
                  (setq beginning-of-innermost-sexp (nth 1 (syntax-ppss)))
                  (goto-char (1+ beginning-of-innermost-sexp))
                  (setq fnword (current-word 'strict))
                  (setq fnsym (intern fnword))
                  (setq depth (1+ depth)))
        (setq found-arx-form (get fnsym 'arx-name))
        (goto-char beginning-of-innermost-sexp))
      (cons found-arx-form depth))))


(defun arx--get-form-func (arx-name sym)
  "In arx ARX-NAME find :func form for SYM (or nil if if doesn't exist).

Resolves all aliases on the way."
  ;; FIXME: add support for builtin rx forms.
  (let ((form-defs
         (get (intern (concat arx-name "-constituents")) 'arx-form-defs))
        sym-defn
        found-form-func)
    (while (and (not found-form-func)
                sym
                (setq sym-defn (cadr (assq sym form-defs))))
      (cond
       ((eq (car-safe sym-defn) :func)
        (setq found-form-func (cadr sym-defn)))
       ((symbolp sym-defn)
        (setq sym sym-defn))
       (t
        (setq sym nil))))
    found-form-func))


(defun arx--get-args-string (func sym index)
  "Return highlighted args string for FUNC referred by SYM at INDEX'th arg."
  (let ((arglist (help-function-arglist func)))
    (elisp--highlight-function-argument
     sym (elisp-function-argstring (cdr arglist)) index
     (concat (propertize (symbol-name sym) 'face 'font-lock-function-name-face)
             ": "))))


(defun arx-documentation-function ()
  "Return current rx form as required for eldoc."
  (let* ((name-and-depth (arx--name-and-depth))
         (fnsym (and (car name-and-depth)
                     (> (cdr name-and-depth) 1)
                     (arx--fnsym-in-current-sexp)))
         (func (and (car fnsym)
                    (arx--get-form-func (car name-and-depth) (car fnsym)))))

    (when func
      (arx--get-args-string func (car fnsym) (cadr fnsym)))))


(define-minor-mode arx-minor-mode
  "Toggle arx minor mode.

When arx-minor-mode is enabled eldoc is hinted to return help for
known arx forms."
  :lighter "[arx]"
  (if arx-minor-mode
      (add-function :before-until (local 'eldoc-documentation-function)
                    #'arx-documentation-function)
    (remove-function (local 'eldoc-documentation-function)
                     #'arx-documentation-function)))

(defun arx--make-macro-constituents-docstring (macro-name)
  "Format docstring for MACRO-NAME -constituents var."
  (format "\
List of form definitions for `%s' and `%s-to-string' functions.

See `%s' for a human readable list of defined forms.

See variable `rx-constituents' for more information about format
of elements of this list."
          macro-name macro-name macro-name))


(defun arx--make-macro-to-string-docstring (macro-name)
  "Format docstring for MACRO-NAME -to-string function."
  (format "\
Parse and produce code for regular expression FORM.

FORM is a regular expression in sexp form as supported by `%s'.
NO-GROUP non-nil means don't put shy groups around the result."
          macro-name))

(defun arx--make-macro-docstring (macro-name form-docstrings)
  "Format docstring for MACRO-NAME macro that defines extra FORM-DOCSTRINGS."
  (apply
   #'concat
   `("\
Translate regular expressions REGEXPS in sexp form to a regexp string.

See macro `rx' for more documentation on REGEXPS parameter.
"
     ,@(if (not form-docstrings)
           '("\n")
         (append '("\
This macro additionally supports the following forms:

")
                 (mapcar (lambda (doc) (concat doc "\n\n")) form-docstrings)))
     ,(format "\
Use function `%s-to-string' to do such a translation at run-time."
             macro-name))))


(defun define-arx--fn (macro form-defs)
  "Implementation for `define-arx' for MACRO and FORM-DEFS."
  (let* ((macro-name (symbol-name macro))
         (macro-to-string (intern (concat macro-name "-to-string")))
         (macro-constituents (intern (concat macro-name "-constituents")))
         form-extra-constituents form-docstrings)
    ;; Preprocess the definitions
    (setq form-defs (delq nil form-defs))
    (setq form-extra-constituents (mapcar #'arx--to-rx form-defs))
    (setq form-docstrings (mapcar #'arx--form-make-docstring form-defs))
    `(eval-and-compile
       ;; Define MACRO-constituents variable.
       (defvar ,macro-constituents
         nil
         ,(arx--make-macro-constituents-docstring macro-name))
       ;; Set MACRO-constituents value in setq so as to refresh
       ;; constituents when re-evaluating define-arx.
       (setq ,macro-constituents
             (append rx-constituents (quote ,form-extra-constituents)))

       ;; Define MACRO-to-string function.
       (defun ,macro-to-string (form &optional no-group)
         ,(arx--make-macro-to-string-docstring macro-name)
         (let ((rx-constituents ,macro-constituents))
           (rx-to-string form no-group)))

       ;; Define MACRO.
       (defmacro ,macro (&rest regexps)
         ,(arx--make-macro-docstring macro-name form-docstrings)
         (cond ((null regexps)
                (error "No regexp"))
               ((cdr regexps)
                (,macro-to-string `(and ,@regexps) t))
               (t
                (,macro-to-string (car regexps) t))))

       ;; Mark macro & function for future reference.
       (put (quote ,macro-constituents) 'arx-form-defs (quote ,form-defs))
       (put (quote ,macro-to-string) 'arx-name ,macro-name)
       (put (quote ,macro) 'arx-name ,macro-name)

       ;; Return value is the macro symbol.
       (quote ,macro))))



;;;###autoload
(defmacro define-arx (macro form-defs)
  "Generate a custom rx-like macro under name MACRO.

See `rx' for how the generated macro can be invoked.

FORM-DEFS is a list of custom s-exp definitions to create whose
elements have the form (SYM DEF), where DEF is one of
the following:

- \"LITERAL\" -- create a matcher to match a string literally

- (regexp \"LITERAL\") -- create a match given a regexp

- SYMBOL -- create an alias for a symbol either defined earlier
  on the list or provided by `rx'

- (SUBFORM ...) -- create an alias for an application of s-exp
  subform either defined earlier on the list or provided by `rx'

- (:func #'FORM-FUNC ...) -- create an s-exp definition

The most interesting here is the last variant.  When a
corresponding rx form will be encountered, FORM-FUNC will be
called with all elements of that form as arguments (with the
first one being the form symbol itself).  FORM-FUNC must then
return a valid s-exp or a properly grouped plain regexp.

Another keywords that are recognized in the plist are:
- :min-args -- minimum number of arguments for that form (default nil)
- :max-args -- maximum number of arguments for that form (default nil)
- :predicate -- if given, all rx form arguments must satisfy it"
  (define-arx--fn macro (eval form-defs)))

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

(defvar reb-buffer)
(autoload 'reb-change-syntax "re-builder")

;;;###autoload
(defun arx-builder (&optional arx-name)
  "Run `re-builder' using arx form named ARX-NAME."
  (interactive
   (list (completing-read
          "Select arx form: "
          (let (l) (mapatoms (lambda (x)
                               (when (equal (symbol-name x)
                                            (get x 'arx-name))
                                 (push x l))))
               l)
          nil t)))

  (let* ((reb-buffer-name (format "*arx-builder(%s)*" arx-name)))

    (let ((reb-buffer reb-buffer-name))
      (condition-case nil
          (re-builder)
        (error))
      (condition-case nil
          (reb-change-syntax 'rx)
        (error)))
    (set (make-local-variable 'reb-buffer) reb-buffer-name)
    (set (make-local-variable 'rx-constituents)
         (symbol-value (intern (concat arx-name "-constituents"))))))





(provide 'ample-regexps)

;;; ample-regexps.el ends here
