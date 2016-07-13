;;; elisp-type-lint--type-table.el --- Define type table and predicates.

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

(defvar elisp-type-lint-type-table nil
  "A map of symbols to symbols.

Use the accessor functions `elisp-type-lint-query' and
`elisp-type-lint-add' instead of accessing the variable directly.
The representation of this variable is up to change, but the spec
for these functions will not.")

(defun elisp-type-lint-query (symbol)
  "Search the type tables for SYMBOL and return its pair.

The type tables are `elisp-type-lint-local-type-table', then
`elisp-type-lint-type-table'.

If SYMBOL isn't found, return nil."
  (elisp-type-lint--assert (symbolp symbol))
  (let ((elisp-type-lint---query
         (lambda ()
           (while (not (null table))
             (if (equal symbol (caar table))
                 (throw 'done (cdar table))
               (setq table (cdr table)))))))
    (catch 'done
      (or
       ;; if local type table is unbound, will do nothing
       (let ((table (ignore-errors elisp-type-lint-local-type-table)))
         (funcall elisp-type-lint---query))
       (let ((table elisp-type-lint-type-table))
         (funcall elisp-type-lint---query))))))

(defun elisp-type-lint-add (symbol type)
  "Add to `elisp-type-lint-type-table' a pair of SYMBOL and TYPE."
  (elisp-type-lint--assert (symbolp symbol))
  (elisp-type-lint--assert (elisp-type-lint-type-p type))
  (setq elisp-type-lint-type-table
        (cons (cons symbol type)
              elisp-type-lint-type-table)))

(provide 'elisp-type-lint--type-table)
;;; elisp-type-lint--type-table.el ends here
