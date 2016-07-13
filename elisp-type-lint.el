;;; elisp-type-lint.el --- Lint Emacs Lisp based on the types of functions.

;; Copyright (C) 2016 Chris Gregory czipperz@gmail.com

;; This file is part of Elisp Type Lint.
;;
;; Elisp Type Lint is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Elisp Type Lint is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Elisp Type Lint.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Chris Gregory "czipperz"
;; Email: czipperz@gmail.com
;; Version: 0.0.1

;;; Commentary:

;;; Usage commentary:
;; Use the function `elisp-type-lint-buffer' to lint the buffer for
;; type inconsistencies.
;;
;; Use the variable `elisp-type-lint-load-path' to customize where
;; `require's will look.

;;; Implementation Commentary:
;;; Types:
;; Type is one of:
;;
;; int  --  integral literal, character literal
;; float  --  floating literal
;; string  --  string literal
;; null  --  null literal
;; t  --  t literal
;; symbol  --  quoted variable
;; (cons Type Type)  --  cons pair, list
;; (quoted Type)  --  quoted other type
;; (fun Type Type...)
;;   --  function definition that returns car and takes cdr
;; (or Type Type Type...)  --  differing types (2+ args)
;; (let typename (templates...) Type)
;;   --  typedef typename to Type. Allows recursive definitions and
;;       templated definitions (list of a).
;; (impl typename (templates...))
;;   --  instantiation of a let
;;
;; The outmost Type will be quoted (so you would write "'int" and
;; "'(cons int float)") to avoid variable collisions.
;;
;; List of a:
;; '(let list (a) (or null (cons a (impl list (a)))))
;; Haskell: data List a = Null | Cons a (List a)
;;
;; Binary tree:
;; '(let tree (a) (or a (cons (impl tree (a)) (impl tree (a)))))
;; Haskell: data Tree a = Item a | Branch (Tree a) (Tree a)

;;; Code:

(defmacro elisp-type-lint--assert (exp &optional message should-format)
  "Assert EXP is truthy, throwing an error if it didn't.

The error message is MESSAGE if it is given.
MESSAGE will be formated with EXP if SHOULD-FORMAT is non nil.

If MESSAGE is null, the error message is \"Assertion failed: %s\", and
SHOULD-FORMAT is t."
  `(unless ,exp
     (error
      ,(if message
           (if should-format
               (format message exp)
             message)
         (format "Assertion failed: %s" exp)))))

(defun elisp-type-lint--get-region (mark point)
  "Get region MARK POINT."
  (interactive (list (mark) (point)))
  (save-excursion (let ((reg (get-register ?r)))
                    (copy-to-register ?r mark point)
                    (prog1 (get-register ?r)
                      (set-register ?r reg)))))

(require 'elisp-type-lint--list-predicates)
(require 'elisp-type-lint--type-predicates)
(require 'elisp-type-lint--type-table)

(defun elisp-type-lint-get-type-region (beg end)
  "Get Type of the sexp at BEG to END."
  (save-excursion
    (goto-char beg)
    (cond ((= beg end)
           nil)
          ((= (char-after) ?\')
           'symbol)
          ((= (char-after) ??)
           'int)
          ((= (char-after) ?\()
           (forward-char)
           (forward-sexp)
           (let ((reg (elisp-type-lint--get-region (1+ beg) (point))))
             (or
              (elisp-type-lint-query (intern reg))
              (error "Unbound function %s" reg))))
          (t
           nil))))

(defun elisp-type-lint-get-type ()
  "Get type of the current sexp at point."
  (save-excursion
    (forward-sexp)
    (let ((e (point)))
      (backward-sexp)
      (elisp-type-lint-get-type-region (point) e))))

(defun elisp-type-lint-buffer ()
  "Lint the current buffer, returning a list of point, message pairs."
  (save-excursion
    (let ((elisp-error-list nil))
      (goto-char (point-min)))))

(provide 'elisp-type-lint)
;;; elisp-type-lint.el ends here
