;;; type-table-tests.el --- Test the functions for accessing the type table.

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
(require 'elisp-type-lint)
(require 'ert)

(ert-deftest elisp-type-lint-add--type-table-1 ()
  (let ((elisp-type-lint-type-table nil))
    (elisp-type-lint-add 'a '(fun int))
    (should (equal elisp-type-lint-type-table
                   '((a . (fun int)))))

    (elisp-type-lint-add 'b '(fun int int int))
    (should (equal elisp-type-lint-type-table
                   '((b . (fun int int int))
                     (a . (fun int)))))))

(ert-deftest elisp-type-lint-query--type-table-2 ()
  (let ((elisp-type-lint-type-table '((a . char)
                                      (b . int))))
    (should (equal (elisp-type-lint-query 'a)
                   'char))
    (should (equal (elisp-type-lint-query 'b)
                   'int))))

(provide 'type-table-tests)
;;; type-table-tests.el ends here
