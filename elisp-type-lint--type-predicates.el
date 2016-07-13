;;; elisp-type-lint--type-predicates.el --- Predicates for types

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
;;; Code:
(require 'elisp-type-lint--list-predicates)

(defun elisp-type-lint-type-int-p (val)
  "Test if VAL is a int type."
  (equal val 'int))

(defun elisp-type-lint-type-float-p (val)
  "Test if VAL is a float type."
  (equal val 'float))

(defun elisp-type-lint-type-string-p (val)
  "Test if VAL is a string type."
  (equal val 'string))

(defun elisp-type-lint-type-null-p (val)
  "Test if VAL is a null type."
  (equal val 'null))

(defun elisp-type-lint-type-t-p (val)
  "Test if VAL is a t type."
  (equal val 't))

(defun elisp-type-lint-type-cons-p (val)
  "Test if VAL is a cons type."
  (and (listp val)
       (equal (car val) 'cons)
       (= (length val) 3)
       (elisp-type-lint--all 'elisp-type-lint-type-p (cdr val))))

(defun elisp-type-lint-type-quoted-p (val)
  "Test if VAL is a quoted type."
  (and (listp val)
       (equal (car val) 'quoted)
       (= (length val) 2)
       (elisp-type-lint--all 'elisp-type-lint-type-p (cdr val))))

(defun elisp-type-lint-type-fun-p (val)
  "Test if VAL is a fun type."
  (and (listp val)
       (equal (car val) 'fun)
       (>= (length val) 2)
       (elisp-type-lint--all 'elisp-type-lint-type-p (cdr val))))

(defun elisp-type-lint-type-or-p (val)
  "Test if VAL is a or type.

Example: (or int float)"
  (and (listp val)
       (equal (car val) 'or)
       (>= (length val) 3)
       (elisp-type-lint--all 'elisp-type-lint-type-p (cdr val))))

(defun elisp-type-lint-type-let-p (val)
  "Test if VAL is a let type.

Example: (let list (a) (or null (cons a (impl list (a)))))"
  (and (listp val)
       (equal (nth 0 val) 'let)
       (= (length val) 4)
       (symbolp (nth 1 val))
       (listp (nth 2 val))
       (let ((elisp-type-lint-local-type-table
              (cons (cons (nth 1 val) val)
                    (ignore-errors
                      elisp-type-lint-local-type-table))))
         (elisp-type-lint-type-p (nth 3 val)))))

(defun elisp-type-lint-type-impl-p (val)
  "Test if VAL is a impl type.

Example: `(impl id (a))'.

Does not test the validity of the type implementation.
Use `elisp-type-lint-type-valid-impl-p' to do that."
  (and (listp val)
       (equal (nth 0 val) 'impl)
       (= (length val) 3)
       (symbolp (nth 1 val))
       (listp (nth 2 val))
       (elisp-type-lint--all 'elisp-type-lint-type-p (nth 2 val))))

(defun elisp-type-lint-type-valid-impl-p (val)
  "Test if VAL is a valid impl type.

Example: `(impl id (a))'.

Returns t when `(let id (a) a)' is in
`elisp-type-lint-local-type-table' or `elisp-type-lint-type-table'.

Returns nil when `(let id () int)' is in
`elisp-type-lint-local-type-table' or `elisp-type-lint-type-table'.
This is because the templated argument lengths don't match."
  (and (elisp-type-lint-type-impl-p val)
       (let ((q (elisp-type-lint-query (nth 1 val))))
         (and
          (elisp-type-lint-type-let-p q)
          ;; correct template size
          (= (length (nth 2 val))
             (length (nth 2 q)))
          (elisp-type-lint-type-p (nth 3 q))))))

(defun elisp-type-lint-type-table-p (val)
  "Test if VAL is defined in a type table."
  nil
  ;; (not (equal nil (elisp-type-lint-query val)))
  )

(defun elisp-type-lint-type-p (val)
  "Test if VAL is a type."
  (or (elisp-type-lint-type-int-p val)
      (elisp-type-lint-type-float-p val)
      (elisp-type-lint-type-string-p val)
      (elisp-type-lint-type-null-p val)
      (elisp-type-lint-type-t-p val)
      (elisp-type-lint-type-cons-p val)
      (elisp-type-lint-type-quoted-p val)
      (elisp-type-lint-type-fun-p val)
      (elisp-type-lint-type-or-p val)
      (elisp-type-lint-type-let-p val)
      (elisp-type-lint-type-impl-p val)
      (elisp-type-lint-type-table-p val)))

(provide 'elisp-type-lint--type-predicates)
;;; elisp-type-lint--type-predicates.el ends here
