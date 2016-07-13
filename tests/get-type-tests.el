;;; get-type-tests.el ---

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
(require 'test-utils)

(deftest elisp-type-lint-get-type-region--get-type-1
    "'asdf"
  (should (equal 'symbol
                 (elisp-type-lint-get-type-region (point-min)
                                                  (point-max)))))

(deftest elisp-type-lint-get-type-region--get-type-2
    "?a"
  (should (equal 'int
                 (elisp-type-lint-get-type-region (point-min)
                                                  (point-max)))))

(deftest elisp-type-lint-get-type-region--get-type-3
    "3.0"
  (should (equal 'float
                 (elisp-type-lint-get-type-region (point-min)
                                                  (point-max)))))

(deftest elisp-type-lint-get-type-region--get-type-4
    ".0"
  (should (equal 'float
                 (elisp-type-lint-get-type-region (point-min)
                                                  (point-max)))))

(deftest elisp-type-lint-get-type-region--get-type-5
    "3."
  (should (equal 'float
                 (elisp-type-lint-get-type-region (point-min)
                                                  (point-max)))))

(deftest elisp-type-lint-get-type-region--get-type-6
    "30"
  (should (equal 'int
                 (elisp-type-lint-get-type-region (point-min)
                                                  (point-max)))))

(deftest elisp-type-lint-get-type-region--get-type-7
    "0"
  (should (equal 'int
                 (elisp-type-lint-get-type-region (point-min)
                                                  (point-max)))))

(deftest elisp-type-lint-get-type-region--get-type-8
    "3"
  (should (equal 'int
                 (elisp-type-lint-get-type-region (point-min)
                                                  (point-max)))))

(deftest elisp-type-lint-get-type-region--get-type-9
    "(or 1 3)"
  (should (equal 'int ;; 1
                 (elisp-type-lint-get-type-region 5 6)))
  (should (equal 'int ;; 3
                 (elisp-type-lint-get-type-region 7 8)))
  (should (equal '(or int null)
                 (let ((elisp-type-lint-local-type-table
                        '((or . (fun (or int null) int int)))))
                   (elisp-type-lint-get-type-region (point-min)
                                                    (point-max))))))

(provide 'get-type-tests)
;;; get-type-tests.el ends here
